use crate::core::interpreter::{Interpreter, ExecutionState};
use std::collections::HashMap;

#[derive(Clone)]
struct CallFrame {
    tokens: Vec<String>,
    pc: usize,
}

pub struct ForthInterpreter {
    stack: Vec<i32>,
    words: HashMap<String, Vec<String>>,
    compiling: bool,
    new_word_name: String,
    new_word_body: Vec<String>,
    
    // Execution state
    call_stack: Vec<CallFrame>,
    loop_stack: Vec<(i32, i32, usize)>, // (index, limit, start_pc)
    state: ExecutionState,
    output_buffer: String,
    waiting_for_input: bool,
}

impl ForthInterpreter {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            words: HashMap::new(),
            compiling: false,
            new_word_name: String::new(),
            new_word_body: Vec::new(),
            call_stack: Vec::new(),
            loop_stack: Vec::new(),
            state: ExecutionState::Finished,
            output_buffer: String::new(),
            waiting_for_input: false,
        }
    }

    // Better tokenizer for Forth strings
    fn tokenize_forth(&self, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();
        
        while let Some(&c) = chars.peek() {
            if c.is_whitespace() {
                chars.next();
            } else {
                // Check for ." 
                // We need to look ahead.
                let mut temp = String::new();
                // Read word
                while let Some(&nc) = chars.peek() {
                    if nc.is_whitespace() { break; }
                    temp.push(chars.next().unwrap());
                }
                
                if temp == ".\"" {
                    tokens.push(temp);
                    // Consume space
                    if let Some(&nc) = chars.peek() {
                        if nc.is_whitespace() { chars.next(); }
                    }
                    // Read until "
                    let mut s = String::new();
                    while let Some(&nc) = chars.peek() {
                        if nc == '"' {
                            chars.next();
                            break;
                        }
                        s.push(chars.next().unwrap());
                    }
                    tokens.push(s); // Push string content as next token
                } else {
                    tokens.push(temp);
                }
            }
        }
        tokens
    }

    fn execute_step(&mut self) {
        if self.call_stack.is_empty() {
            self.state = ExecutionState::Finished;
            return;
        }

        // Get mutable reference to current frame
        let frame_idx = self.call_stack.len() - 1;
        let frame = &mut self.call_stack[frame_idx];

        if frame.pc >= frame.tokens.len() {
            // Pop frame
            self.call_stack.pop();
            if self.call_stack.is_empty() {
                self.state = ExecutionState::Finished;
            }
            return;
        }

        let token = frame.tokens[frame.pc].clone();
        let upper = token.to_uppercase();
        frame.pc += 1; // Advance PC

        if self.compiling {
            if token == ";" {
                self.words.insert(self.new_word_name.clone(), self.new_word_body.clone());
                self.compiling = false;
                self.new_word_name.clear();
                self.new_word_body.clear();
            } else if self.new_word_name.is_empty() {
                self.new_word_name = token.to_string();
            } else {
                self.new_word_body.push(token.to_string());
            }
            return;
        }

        if token == ":" {
            self.compiling = true;
            return;
        }

        // Check if it's a defined word
        if self.words.contains_key(&token) {
            let body = self.words.get(&token).unwrap().clone();
            // Push new frame
            self.call_stack.push(CallFrame {
                tokens: body,
                pc: 0,
            });
            return;
        }

        // Numbers
        if let Ok(num) = token.parse::<i32>() {
            self.stack.push(num);
            return;
        }

        match upper.as_str() {
            "+" => {
                if self.stack.len() >= 2 {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                }
            },
            "-" => {
                if self.stack.len() >= 2 {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                }
            },
            "*" => {
                if self.stack.len() >= 2 {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a * b);
                }
            },
            "/" => {
                if self.stack.len() >= 2 {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    if b != 0 { self.stack.push(a / b); }
                }
            },
            "." => {
                if let Some(val) = self.stack.pop() {
                    self.output_buffer.push_str(&format!("{} ", val));
                }
            },
            ".\"" => {
                // Next token is the string
                // We need to access the frame again since we borrowed it mutably before
                // But we dropped the borrow to match.
                // Let's re-access.
                let frame = &mut self.call_stack[frame_idx];
                if frame.pc < frame.tokens.len() {
                    self.output_buffer.push_str(&frame.tokens[frame.pc]);
                    frame.pc += 1;
                }
            },
            "CR" => self.output_buffer.push('\n'),
            "DUP" => {
                if let Some(&val) = self.stack.last() {
                    self.stack.push(val);
                }
            },
            "DROP" => { self.stack.pop(); },
            "SWAP" => {
                if self.stack.len() >= 2 {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(b);
                    self.stack.push(a);
                }
            },
            "IF" => {
                // Pop stack. If 0, jump to ELSE or THEN
                if let Some(val) = self.stack.pop() {
                    if val == 0 {
                        // Skip to ELSE or THEN
                        let frame = &mut self.call_stack[frame_idx];
                        let mut depth = 1;
                        while frame.pc < frame.tokens.len() {
                            let t = frame.tokens[frame.pc].to_uppercase();
                            if t == "IF" { depth += 1; }
                            if t == "THEN" { depth -= 1; }
                            if t == "ELSE" && depth == 1 { 
                                // Found matching ELSE
                                frame.pc += 1; // Skip ELSE
                                break;
                            }
                            if depth == 0 { 
                                // Found matching THEN
                                // frame.pc is already at THEN (because we incremented before check? No, we are iterating)
                                // Wait, we are iterating frame.pc
                                break; 
                            }
                            frame.pc += 1;
                        }
                    }
                    // If val != 0, just continue (execute IF block)
                }
            },
            "ELSE" => {
                // If we hit ELSE during execution, it means we executed the IF part.
                // So we must skip to THEN.
                let frame = &mut self.call_stack[frame_idx];
                let mut depth = 1;
                while frame.pc < frame.tokens.len() {
                    let t = frame.tokens[frame.pc].to_uppercase();
                    if t == "IF" { depth += 1; }
                    if t == "THEN" { 
                        depth -= 1;
                        if depth == 0 { 
                            break; 
                        }
                    }
                    frame.pc += 1;
                }
            },
            "THEN" => {
                // Just a marker, do nothing
            },
            "DO" => {
                // limit start DO
                if self.stack.len() >= 2 {
                    let start = self.stack.pop().unwrap();
                    let limit = self.stack.pop().unwrap();
                    // We need to store the PC *after* DO, which is current frame.pc (since we already incremented)
                    let frame = &self.call_stack[frame_idx];
                    self.loop_stack.push((start, limit, frame.pc));
                }
            },
            "LOOP" => {
                if let Some((mut index, limit, start_pc)) = self.loop_stack.pop() {
                    index += 1;
                    if index < limit {
                        self.loop_stack.push((index, limit, start_pc));
                        let frame = &mut self.call_stack[frame_idx];
                        frame.pc = start_pc; // Jump back
                    }
                }
            },
            "I" => {
                if let Some((index, _, _)) = self.loop_stack.last() {
                    self.stack.push(*index);
                }
            },
            "KEY" => {
                self.waiting_for_input = true;
                self.state = ExecutionState::WaitingForInput("Key input required".to_string());
                // We need to decrement PC so we re-execute KEY when resumed?
                // No, when resumed, we will have the input.
                // But we need to know we are waiting for input.
                // Actually, better pattern:
                // Set state to WaitingForInput.
                // Return.
                // When provide_input is called, push input to stack, set state to Running.
                // But we already advanced PC. So next step will be next instruction.
                // This is correct.
            },
            "EMIT" => {
                if let Some(val) = self.stack.pop() {
                    if let Some(c) = std::char::from_u32(val as u32) {
                        self.output_buffer.push(c);
                    }
                }
            },
            _ => {
                // Unknown word
                self.output_buffer.push_str(&format!("❌ Unknown word: {}\n", token));
            }
        }
    }

    fn execute_steps(&mut self, steps: usize) {
        let mut steps_executed = 0;
        while steps_executed < steps && self.state == ExecutionState::Running {
            self.execute_step();
            steps_executed += 1;
        }
    }

    fn get_output_internal(&mut self) -> String {
        let output = self.output_buffer.clone();
        self.output_buffer.clear();
        output
    }
}

impl Interpreter for ForthInterpreter {
    fn execute(&mut self, command: &str) -> String {
        let (output, _) = self.start_execution(command);
        // If it's not finished, we should probably continue until finished or waiting
        let mut final_output = output;
        while self.state == ExecutionState::Running {
             let (out, _) = self.continue_execution();
             final_output.push_str(&out);
        }
        final_output
    }

    fn run(&mut self, code: &str) -> String {
        self.stack.clear();
        self.words.clear();
        self.compiling = false;
        self.execute(code)
    }

    fn start_execution(&mut self, code: &str) -> (String, ExecutionState) {
        let tokens = self.tokenize_forth(code);
        self.call_stack.clear();
        self.call_stack.push(CallFrame {
            tokens,
            pc: 0,
        });
        self.loop_stack.clear();
        self.state = ExecutionState::Running;
        self.output_buffer.clear();
        self.waiting_for_input = false;
        
        self.execute_steps(100);
        (self.get_output_internal(), self.state.clone())
    }

    fn continue_execution(&mut self) -> (String, ExecutionState) {
        if self.state == ExecutionState::Finished {
            return (String::new(), ExecutionState::Finished);
        }
        // If we were waiting for input and got it, we are already Running (set in provide_input)
        // If we are just continuing, ensure we are Running
        if let ExecutionState::WaitingForInput(_) = self.state {
             // Still waiting
             return (String::new(), self.state.clone());
        }
        
        self.state = ExecutionState::Running;
        self.execute_steps(100);
        (self.get_output_internal(), self.state.clone())
    }

    fn step_execution(&mut self) -> (String, ExecutionState) {
        if self.state == ExecutionState::Finished {
            return (String::new(), ExecutionState::Finished);
        }
        if let ExecutionState::WaitingForInput(_) = self.state {
             return (String::new(), self.state.clone());
        }
        
        self.state = ExecutionState::Running;
        self.execute_steps(1);
        (self.get_output_internal(), self.state.clone())
    }

    fn provide_input(&mut self, input: &str) {
        if self.waiting_for_input {
            // For KEY, we take the first char
            if let Some(c) = input.chars().next() {
                self.stack.push(c as i32);
            } else {
                self.stack.push(0); // EOF or empty?
            }
            self.waiting_for_input = false;
            self.state = ExecutionState::Running;
        }
    }

    fn get_variables(&self) -> HashMap<String, String> {
        let mut vars = HashMap::new();
        vars.insert("STACK".to_string(), format!("{:?}", self.stack));
        vars.insert("WORDS_COUNT".to_string(), self.words.len().to_string());
        vars
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_forth_arithmetic() {
        let mut interpreter = ForthInterpreter::new();
        let code = "10 20 + .";
        let output = interpreter.run(code);
        assert_eq!(output, "30 ");
    }

    #[test]
    fn test_forth_if() {
        let mut interpreter = ForthInterpreter::new();
        let code = "1 IF .\" Yes\" ELSE .\" No\" THEN";
        let output = interpreter.run(code);
        assert_eq!(output, "Yes");
        
        let code2 = "0 IF .\" Yes\" ELSE .\" No\" THEN";
        let output2 = interpreter.run(code2);
        assert_eq!(output2, "No");
    }

    #[test]
    fn test_forth_loop() {
        let mut interpreter = ForthInterpreter::new();
        let code = "3 0 DO I . LOOP";
        let output = interpreter.run(code);
        assert_eq!(output, "0 1 2 ");
    }
    
    #[test]
    fn test_forth_word() {
        let mut interpreter = ForthInterpreter::new();
        let code = ": DOUBLE 2 * ; 5 DOUBLE .";
        let output = interpreter.run(code);
        assert_eq!(output, "10 ");
    }
}
