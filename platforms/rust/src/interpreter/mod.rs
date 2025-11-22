//! Time Warp Unified Interpreter
//!
//! Central execution engine supporting PILOT, BASIC, and Logo languages.
//! Handles program loading, execution, variable management, and control flow.
//!
//! # Supported Languages
//! - **PILOT**: Educational language with text/match/jump commands
//! - **BASIC**: Classic PRINT/LET/INPUT/IF/FOR/GOTO (in progress)
//! - **Logo**: Turtle graphics with forward/back/left/right (in progress)
//!
//! # Example
//! ```rust,no_run
//! use time_warp_unified::interpreter::Interpreter;
//! use time_warp_unified::graphics::TurtleState;
//!
//! let mut interp = Interpreter::new();
//! let mut turtle = TurtleState::default();
//!
//! // Load PILOT program
//! interp.load_program("T:Hello\nT:World").unwrap();
//! let output = interp.execute(&mut turtle).unwrap();
//! assert_eq!(output, vec!["Hello", "World"]);
//! ```
//!
//! # Architecture
//! - Stateless language executors in `languages/` modules
//! - Shared state: variables, output buffer, control flow stacks
//! - Regex optimization: Lazy-compiled patterns for 5-10x speedup
//!
//! # Security
//! - Execution timeout: MAX_ITERATIONS=100,000 prevents infinite loops
//! - Expression complexity limits in ExpressionEvaluator
//! - Error recovery: Continues on non-fatal errors

use anyhow::Result;
use std::time::{Duration, Instant};

/// Security limit: Maximum program execution time (10 seconds)
const MAX_EXECUTION_TIME: Duration = Duration::from_secs(10);
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;

use crate::graphics::TurtleState;
use crate::languages::logo::LogoProcedure;
use crate::languages::{basic, logo, pilot, Language};
use crate::utils::error_hints;
use crate::utils::ExpressionEvaluator;

// Type aliases to reduce type complexity in public fields
pub type InputCallback = Box<dyn FnMut(&str) -> String>;
pub type InkeyCallback = Box<dyn Fn() -> Option<String>>;

// Lazy compiled regex for variable interpolation (5-10x performance boost)
static VAR_INTERPOLATION_PATTERN: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\*([A-Z_][A-Z0-9_]*)\*").expect("Invalid regex pattern"));

/// Execution control flow result
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExecutionResult {
    Continue,
    End,
    Jump(usize),
    /// Pause execution to wait for user input
    WaitForInput,
}

/// Unified screen modes akin to GW-BASIC
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScreenMode {
    /// Text mode with columns and rows (e.g., 80x25)
    Text { cols: u32, rows: u32 },
    /// Graphics mode with pixel dimensions
    Graphics { width: u32, height: u32 },
}

/// Main interpreter managing program state and language dispatch
pub struct Interpreter {
    // Core state
    pub variables: HashMap<String, f64>,
    pub string_variables: HashMap<String, String>,
    pub output: Vec<String>,

    // Program state
    pub program_lines: Vec<(Option<usize>, String)>,
    pub current_line: usize,
    pub labels: HashMap<String, usize>,

    // Line number mapping for BASIC (line_number -> program_lines index)
    pub line_number_map: HashMap<usize, usize>,

    // Control flow stacks
    pub gosub_stack: Vec<usize>,
    pub for_stack: Vec<ForContext>,

    // PILOT-specific
    pub match_flag: bool,
    pub last_match_set: bool,
    pub stored_condition: Option<bool>,

    // Language detection (reserved for future multi-language execution)
    #[allow(dead_code)]
    pub current_language: Language,

    // I/O handling
    pub input_callback: Option<InputCallback>,
    pub last_input: String,

    // Logo procedures (name -> body lines)
    pub logo_procedures: std::collections::HashMap<String, LogoProcedure>,

    // Pending input request (when running in UI without callback)
    pub pending_input: Option<InputRequest>,
    pub pending_resume_line: Option<usize>,

    // Keyboard state for INKEY$ (callback for tests, direct field for UI)
    pub inkey_callback: Option<InkeyCallback>,
    pub last_key_pressed: Option<String>,

    // Unified screen state
    pub screen_mode: ScreenMode,

    // Text buffer for Text screen mode (render target for unified screen)
    pub text_lines: Vec<String>,

    // Text cursor position (row, col) for text mode output
    pub cursor_row: u32,
    pub cursor_col: u32,
}

#[derive(Clone)]
pub struct ForContext {
    #[allow(dead_code)]
    pub var_name: String,
    #[allow(dead_code)]
    pub end_value: f64,
    #[allow(dead_code)]
    pub step: f64,
    #[allow(dead_code)]
    pub for_line: usize,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            string_variables: HashMap::new(),
            output: Vec::new(),

            program_lines: Vec::new(),
            current_line: 0,
            labels: HashMap::new(),
            line_number_map: HashMap::new(),

            gosub_stack: Vec::new(),
            for_stack: Vec::new(),

            match_flag: false,
            last_match_set: false,
            stored_condition: None,

            current_language: Language::Pilot,

            input_callback: None,
            last_input: String::new(),
            logo_procedures: HashMap::new(),
            pending_input: None,
            pending_resume_line: None,
            inkey_callback: None,
            last_key_pressed: None,
            screen_mode: ScreenMode::Graphics {
                width: 800,
                height: 600,
            },
            text_lines: Vec::new(),
            cursor_row: 0,
            cursor_col: 0,
        }
    }

    pub fn load_program(&mut self, program_text: &str) -> Result<()> {
        self.reset();

        let lines: Vec<&str> = program_text.lines().collect();
        self.program_lines.clear();
        self.line_number_map.clear();

        for (idx, line) in lines.iter().enumerate() {
            let (line_num, command_str) = self.parse_line(line);
            let command_owned = command_str.to_string();

            // Build line number mapping for BASIC GOTO/GOSUB
            if let Some(num) = line_num {
                self.line_number_map.insert(num, idx);
            }

            // Collect PILOT labels before pushing
            if let Some(stripped) = command_owned.strip_prefix("L:") {
                let label = stripped.trim();
                self.labels.insert(label.to_string(), idx);
            }

            self.program_lines.push((line_num, command_owned));
        }

        Ok(())
    }

    /// Execute a loaded program with error recovery and timeout protection
    ///
    /// Continues execution on non-fatal errors, collecting error messages in output.
    /// Stops on fatal errors (infinite loops, timeouts, stack overflows).
    ///
    /// # Arguments
    /// * `turtle` - Graphics state for turtle commands
    ///
    /// # Returns
    /// * `Ok(Vec<String>)` - Program output (text and error messages)
    /// * `Err` - Fatal execution error (e.g., timeout, max iterations exceeded)
    ///
    /// # Security
    /// - Max iterations: 100,000 (prevents infinite loops)
    /// - Max execution time: 10 seconds (prevents DoS)
    pub fn execute(&mut self, turtle: &mut TurtleState) -> Result<Vec<String>> {
        // Only reset output at the start of a fresh run. When resuming after input,
        // preserve previous output and current_line set by provide_input().
        if self.current_line == 0 {
            self.output.clear();
        }

        let max_iterations = 100000;
        let mut iterations = 0;
        let start_time = Instant::now();

        while self.current_line < self.program_lines.len() && iterations < max_iterations {
            // Security check: Timeout protection
            if start_time.elapsed() > MAX_EXECUTION_TIME {
                self.log_output("❌ Error: Execution timeout (10 seconds exceeded)".to_string());
                return Err(anyhow::anyhow!("Execution timeout exceeded"));
            }

            iterations += 1;

            // Clone command to avoid borrow checker issues with execute_line
            let command = self.program_lines[self.current_line].1.clone();

            if command.trim().is_empty() {
                self.current_line += 1;
                continue;
            }

            // Error recovery: Continue on non-fatal errors
            let result = match self.execute_line(&command, turtle) {
                Ok(res) => res,
                Err(e) => {
                    // Enhanced error message with context and suggestions
                    let mut error_msg =
                        format!("❌ Error at line {}: {}", self.current_line + 1, e);

                    // Check for syntax mistakes
                    let syntax_hints = error_hints::check_syntax_mistakes(&command);
                    if !syntax_hints.is_empty() {
                        error_msg.push_str(&format!("\n   💡 Hint: {}", syntax_hints.join(", ")));
                    }

                    // Suggest command corrections for unknown commands
                    if e.to_string().contains("Unknown") || e.to_string().contains("Invalid") {
                        let first_word = command.split_whitespace().next().unwrap_or("");
                        if let Some(suggestion) = error_hints::suggest_command(first_word) {
                            error_msg.push_str(&format!("\n   💡 {}", suggestion));
                        }
                    }

                    self.log_output(error_msg);
                    self.current_line += 1;
                    continue;
                }
            };

            match result {
                ExecutionResult::Continue => self.current_line += 1,
                ExecutionResult::End => break,
                ExecutionResult::Jump(line) => self.current_line = line,
                ExecutionResult::WaitForInput => {
                    // Pause execution; UI should collect input and call provide_input()
                    break;
                }
            }
        }

        if iterations >= max_iterations {
            self.log_output("⚠️ Warning: Maximum iterations reached".to_string());
        }

        // Return reference to avoid cloning output vector
        Ok(self.output.clone())
    }

    /// Get reference to output without cloning (for performance-critical code)
    #[allow(dead_code)]
    pub fn get_output(&self) -> &[String] {
        &self.output
    }

    fn execute_line(&mut self, command: &str, turtle: &mut TurtleState) -> Result<ExecutionResult> {
        let cmd_type = self.determine_command_type(command);

        match cmd_type {
            Language::Pilot => pilot::execute(self, command, turtle),
            Language::Basic => basic::execute(self, command, turtle),
            Language::Logo => logo::execute(self, command, turtle),
            _ => Ok(ExecutionResult::Continue),
        }
    }

    fn determine_command_type(&self, command: &str) -> Language {
        let cmd = command.trim();

        // PILOT: commands start with letter followed by colon
        if cmd.len() > 1 && cmd.chars().nth(1) == Some(':') {
            return Language::Pilot;
        }

        let first_word = cmd.split_whitespace().next().unwrap_or("");
        let first_upper = first_word.to_uppercase();

        // Check Logo procedures first (user-defined takes precedence over BASIC keywords)
        if self.logo_procedures.contains_key(&first_upper) {
            return Language::Logo;
        }

        // Logo keywords (expanded)
        let logo_keywords = [
            "FORWARD",
            "FD",
            "BACK",
            "BK",
            "LEFT",
            "LT",
            "RIGHT",
            "RT",
            "PENUP",
            "PU",
            "PENDOWN",
            "PD",
            "CLEARSCREEN",
            "CS",
            "HOME",
            "SETXY",
            "REPEAT",
            "TO",
            "END",
            "SETHEADING",
            "SETH",
            "SETCOLOR",
            "SETPENCOLOR",
            "PENWIDTH",
            "SETPENSIZE",
            "SETBGCOLOR",
            "HIDETURTLE",
            "HT",
            "SHOWTURTLE",
            "ST",
        ];
        if logo_keywords.contains(&first_upper.as_str()) {
            return Language::Logo;
        }

        // BASIC keywords
        let basic_keywords = [
            "LET", "PRINT", "INPUT", "GOTO", "IF", "THEN", "FOR", "NEXT", "GOSUB", "RETURN", "REM",
            "DIM", "DATA", "READ", "LINE", "CIRCLE", "SCREEN", "CLS", "LOCATE",
        ];
        if basic_keywords.contains(&first_upper.as_str()) {
            return Language::Basic;
        }

        // Default to PILOT
        Language::Pilot
    }

    fn parse_line<'a>(&self, line: &'a str) -> (Option<usize>, &'a str) {
        let line = line.trim();

        // Check for line number at start
        let parts: Vec<&str> = line.splitn(2, char::is_whitespace).collect();
        if let Some(first) = parts.first() {
            if let Ok(num) = first.parse::<usize>() {
                if parts.len() > 1 {
                    return (Some(num), parts[1].trim());
                }
            }
        }

        (None, line)
    }

    pub fn log_output(&mut self, text: String) {
        self.output.push(text);
        // Also update text buffer for Text mode rendering
        let max_rows = match self.screen_mode {
            ScreenMode::Text { rows, .. } => rows as usize,
            _ => 0,
        };
        if max_rows > 0 {
            self.text_lines
                .push(self.output.last().cloned().unwrap_or_default());
            while self.text_lines.len() > max_rows {
                self.text_lines.remove(0);
            }
        }
    }

    pub fn evaluate_expression(&self, expr: &str) -> Result<f64> {
        // Use safe expression evaluator
        let eval = ExpressionEvaluator::with_variables(self.variables.clone());
        eval.evaluate(expr)
    }

    /// Interpolate variables in text (e.g., "Hello *NAME*" → "Hello World")
    ///
    /// Fast path: No regex if text contains no asterisks (5-10x faster)
    pub fn interpolate_text(&self, text: &str) -> String {
        // Fast path: Skip regex if no variables to interpolate (5-10x faster)
        if !text.contains('*') {
            return text.to_string();
        }

        // Use captures to avoid multiple regex scans
        let mut result = String::with_capacity(text.len() + 32); // Pre-allocate with some headroom
        let mut last_end = 0;

        for cap in VAR_INTERPOLATION_PATTERN.captures_iter(text) {
            let m = cap.get(0).unwrap();
            result.push_str(&text[last_end..m.start()]);

            let var_name = &cap[1];
            if let Some(val) = self.variables.get(var_name) {
                result.push_str(&val.to_string());
            } else if let Some(val) = self.string_variables.get(var_name) {
                result.push_str(val);
            } else {
                // Keep original *VAR* if not found
                result.push_str(m.as_str());
            }

            last_end = m.end();
        }
        result.push_str(&text[last_end..]);

        result
    }

    fn reset(&mut self) {
        self.variables.clear();
        self.string_variables.clear();
        self.output.clear();
        self.text_lines.clear();
        self.program_lines.clear();
        self.current_line = 0;
        self.labels.clear();
        self.gosub_stack.clear();
        self.for_stack.clear();
        self.match_flag = false;
        self.last_match_set = false;
        self.stored_condition = None;
        self.logo_procedures.clear();
        self.pending_input = None;
        self.pending_resume_line = None;
        self.cursor_row = 0;
        self.cursor_col = 0;
    }

    // Stack operations for GOSUB/RETURN
    pub fn push_gosub(&mut self, line: usize) {
        self.gosub_stack.push(line);
    }

    pub fn pop_gosub(&mut self) -> Option<usize> {
        self.gosub_stack.pop()
    }

    // FOR/NEXT loop management (reserved for BASIC implementation)
    #[allow(dead_code)]
    pub fn push_for(&mut self, var: String, end_val: f64, step: f64, line: usize) {
        self.for_stack.push(ForContext {
            var_name: var,
            end_value: end_val,
            step,
            for_line: line,
        });
    }

    #[allow(dead_code)]
    pub fn pop_for(&mut self) -> Option<ForContext> {
        self.for_stack.pop()
    }

    #[allow(dead_code)]
    pub fn peek_for(&self) -> Option<&ForContext> {
        self.for_stack.last()
    }

    // Jump to label
    pub fn jump_to_label(&self, label: &str) -> Option<usize> {
        self.labels.get(label).copied()
    }

    /// Request input from user (uses callback if set, otherwise returns empty)
    pub fn request_input(&mut self, prompt: &str) -> String {
        if let Some(ref mut callback) = self.input_callback {
            let input = callback(prompt);
            self.last_input = input.clone();
            input
        } else {
            // No callback set, return empty (non-interactive mode)
            String::new()
        }
    }

    /// Initiate a pending input request to be fulfilled by the UI.
    /// Stores the prompt and target variable, and marks current line for resume.
    pub fn start_input_request(&mut self, prompt: &str, var_name: &str, prefer_numeric: bool) {
        // Only create if one isn't already pending
        if self.pending_input.is_none() {
            self.pending_input = Some(InputRequest {
                prompt: prompt.to_string(),
                var_name: var_name.to_string(),
                prefer_numeric,
            });
            self.pending_resume_line = Some(self.current_line);
        }
    }

    /// Provide the user input value to satisfy a pending request; assigns variable and advances.
    pub fn provide_input(&mut self, value: &str) {
        if let Some(req) = self.pending_input.take() {
            self.last_input = value.to_string();
            if req.prefer_numeric {
                if let Ok(num) = value.trim().parse::<f64>() {
                    self.variables.insert(req.var_name.clone(), num);
                } else {
                    self.string_variables
                        .insert(req.var_name.clone(), value.to_string());
                }
            } else {
                // String-first
                if value.trim().is_empty() {
                    self.string_variables
                        .insert(req.var_name.clone(), String::new());
                } else if let Ok(num) = value.trim().parse::<f64>() {
                    self.variables.insert(req.var_name.clone(), num);
                } else {
                    self.string_variables
                        .insert(req.var_name.clone(), value.to_string());
                }
            }
            if let Some(line) = self.pending_resume_line.take() {
                // Advance to next line after the INPUT command
                self.current_line = line + 1;
            }
        }
    }

    /// Get the last key pressed (INKEY$ functionality)
    pub fn get_inkey(&mut self) -> String {
        // Check direct field first (UI mode)
        if let Some(key) = self.last_key_pressed.take() {
            return key;
        }

        // Fall back to callback (test mode)
        if let Some(ref callback) = self.inkey_callback {
            callback().unwrap_or_default()
        } else {
            String::new()
        }
    }
}

/// Describes a pending input request awaiting UI entry
#[derive(Debug, Clone)]
pub struct InputRequest {
    pub prompt: String,
    pub var_name: String,
    pub prefer_numeric: bool,
}
