use crate::core::interpreter::{Interpreter, ExecutionState};
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum BlockType {
    While(usize), // start_pc of the while loop (the 'while' token)
    If,           // Inside an if block
    Else,         // Inside an else block
}

pub struct CInterpreter {
    variables: HashMap<String, i32>,
    tokens: Vec<String>,
    pc: usize,
    block_stack: Vec<BlockType>,
    waiting_for_input_var: Option<String>,
}

impl CInterpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            tokens: Vec::new(),
            pc: 0,
            block_stack: Vec::new(),
            waiting_for_input_var: None,
        }
    }

    fn tokenize(&self, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&c) = chars.peek() {
            if c.is_whitespace() {
                chars.next();
            } else if c.is_alphabetic() || c == '_' {
                let mut ident = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_alphanumeric() || nc == '_' {
                        ident.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                tokens.push(ident);
            } else if c.is_digit(10) {
                let mut num = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_digit(10) {
                        num.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                tokens.push(num);
            } else if c == '"' {
                chars.next(); // skip opening quote
                let mut s = String::new();
                s.push('"'); // keep quotes for identification
                while let Some(&nc) = chars.peek() {
                    if nc == '"' {
                        chars.next();
                        break;
                    }
                    s.push(chars.next().unwrap());
                }
                s.push('"');
                tokens.push(s);
            } else {
                // Symbols
                let mut sym = String::new();
                sym.push(chars.next().unwrap());
                // Check for 2-char symbols: ==, !=, <=, >=, &&, ||
                if let Some(&nc) = chars.peek() {
                    let combined = format!("{}{}", sym, nc);
                    if matches!(combined.as_str(), "==" | "!=" | "<=" | ">=" | "&&" | "||") {
                        chars.next();
                        tokens.push(combined);
                    } else {
                        tokens.push(sym);
                    }
                } else {
                    tokens.push(sym);
                }
            }
        }
        tokens
    }

    fn evaluate_expression(&self, tokens: &[String]) -> i32 {
        // Simple expression evaluator: handles numbers, variables, +, -, *, /, ==, !=, <, >
        // No precedence handling for now, just left-to-right or simple 3-part
        
        if tokens.is_empty() { return 0; }
        
        // If single token
        if tokens.len() == 1 {
            let t = &tokens[0];
            if let Ok(n) = t.parse::<i32>() {
                return n;
            }
            if let Some(&v) = self.variables.get(t) {
                return v;
            }
            return 0;
        }

        // Handle binary ops (very simple, no precedence)
        // A op B
        if tokens.len() >= 3 {
            let op = &tokens[1];
            let left = self.evaluate_expression(&tokens[0..1]);
            let right = self.evaluate_expression(&tokens[2..]);
            
            match op.as_str() {
                "+" => return left + right,
                "-" => return left - right,
                "*" => return left * right,
                "/" => return if right != 0 { left / right } else { 0 },
                "==" => return if left == right { 1 } else { 0 },
                "!=" => return if left != right { 1 } else { 0 },
                "<" => return if left < right { 1 } else { 0 },
                ">" => return if left > right { 1 } else { 0 },
                _ => {}
            }
        }
        
        0
    }

    fn find_matching_brace(&self, start: usize) -> usize {
        let mut depth = 1;
        let mut i = start + 1;
        while i < self.tokens.len() && depth > 0 {
            if self.tokens[i] == "{" { depth += 1; }
            if self.tokens[i] == "}" { depth -= 1; }
            if depth > 0 { i += 1; }
        }
        i
    }

    fn find_semicolon(&self, start: usize) -> usize {
        let mut i = start;
        while i < self.tokens.len() && self.tokens[i] != ";" {
            i += 1;
        }
        i
    }

    fn execute_steps(&mut self, steps: usize) -> (String, ExecutionState) {
        let mut output = String::new();
        
        if let Some(var) = &self.waiting_for_input_var {
            return (output, ExecutionState::WaitingForInput(var.clone()));
        }

        for _ in 0..steps {
            if self.pc >= self.tokens.len() {
                return (output, ExecutionState::Finished);
            }

            let token = self.tokens[self.pc].clone();

            if token == "int" {
                // int x = 10; or int x;
                if self.pc + 3 < self.tokens.len() && self.tokens[self.pc+2] == "=" {
                    let var_name = self.tokens[self.pc+1].clone();
                    let end = self.find_semicolon(self.pc + 3);
                    let expr = &self.tokens[self.pc+3..end];
                    let val = self.evaluate_expression(expr);
                    self.variables.insert(var_name, val);
                    self.pc = end + 1;
                } else {
                    let var_name = self.tokens[self.pc+1].clone();
                    self.variables.insert(var_name, 0);
                    self.pc += 3; // int x ;
                }
            } else if token == "printf" {
                if self.pc + 2 < self.tokens.len() && self.tokens[self.pc+1] == "(" {
                    let content = &self.tokens[self.pc+2];
                    if content.starts_with('"') {
                        let mut fmt = content[1..content.len()-1].to_string();
                        fmt = fmt.replace("\\n", "\n");
                        
                        if fmt.contains("%d") {
                            if self.pc + 4 < self.tokens.len() && self.tokens[self.pc+3] == "," {
                                let end = self.find_semicolon(self.pc); // Find end of statement
                                // Find closing paren of printf
                                let mut j = self.pc + 4;
                                while j < end && self.tokens[j] != ")" { j += 1; }
                                
                                let val = self.evaluate_expression(&self.tokens[self.pc+4..j]);
                                fmt = fmt.replace("%d", &val.to_string());
                            }
                        }
                        output.push_str(&fmt);
                    }
                    let end = self.find_semicolon(self.pc);
                    self.pc = end + 1;
                }
            } else if token == "scanf" {
                // scanf("%d", &x);
                // Simplified: scanf("%d", &x) -> wait for input for x
                if self.pc + 4 < self.tokens.len() && self.tokens[self.pc+3] == "," {
                    // tokens[pc+4] should be &x or just x? C uses &x.
                    // Let's handle &x as two tokens "&", "x" or one "&x"?
                    // My tokenizer splits symbols. So "&", "x".
                    
                    let mut var_name_idx = self.pc + 4;
                    if self.tokens[var_name_idx] == "&" {
                        var_name_idx += 1;
                    }
                    let var_name = self.tokens[var_name_idx].clone();
                    
                    self.waiting_for_input_var = Some(var_name.clone());
                    
                    // Advance PC past the scanf statement so when we resume we are at next statement
                    let end = self.find_semicolon(self.pc);
                    self.pc = end + 1;
                    
                    return (output, ExecutionState::WaitingForInput(var_name));
                } else {
                    // Malformed scanf, skip
                    let end = self.find_semicolon(self.pc);
                    self.pc = end + 1;
                }
            } else if token == "if" {
                // if (cond) { ... }
                // Find condition
                let mut j = self.pc + 2;
                let mut depth = 1;
                while j < self.tokens.len() && depth > 0 {
                    if self.tokens[j] == "(" { depth += 1; }
                    if self.tokens[j] == ")" { depth -= 1; }
                    if depth > 0 { j += 1; }
                }
                let cond_tokens = &self.tokens[self.pc+2..j];
                let cond_val = self.evaluate_expression(cond_tokens);
                
                let block_start = j + 2; // ) {
                
                if cond_val != 0 {
                    // Enter block
                    self.block_stack.push(BlockType::If);
                    self.pc = block_start;
                } else {
                    // Skip block
                    let block_end = self.find_matching_brace(block_start - 1); // point to {
                    self.pc = block_end + 1;
                    
                    // Check for else
                    if self.pc < self.tokens.len() && self.tokens[self.pc] == "else" {
                        // Enter else block
                        self.block_stack.push(BlockType::Else);
                        // else { -> skip else, {
                        // Check if it is "else if" or "else {"
                        if self.tokens[self.pc+1] == "if" {
                            // Handle else if as just if? 
                            // "else if" -> "else" "if".
                            // If we enter here, we are effectively in an else block that contains an if.
                            // But my stack logic might be too simple.
                            // Let's assume "else {" for now.
                            self.pc += 1; // point to if
                        } else {
                            self.pc += 2; // point inside {
                        }
                    }
                }
            } else if token == "else" {
                // We hit an else instruction. This means we just finished an 'if' block (and fell through).
                // We should skip the else block.
                if self.tokens[self.pc+1] == "{" {
                    let block_end = self.find_matching_brace(self.pc + 1);
                    self.pc = block_end + 1;
                } else if self.tokens[self.pc+1] == "if" {
                     // else if... skip the if statement?
                     // This is getting complicated.
                     // Simple hack: assume braces.
                     self.pc += 1;
                } else {
                     self.pc += 1;
                }
            } else if token == "while" {
                // while (cond) { ... }
                let start_pc = self.pc;
                
                let mut j = self.pc + 2;
                let mut depth = 1;
                while j < self.tokens.len() && depth > 0 {
                    if self.tokens[j] == "(" { depth += 1; }
                    if self.tokens[j] == ")" { depth -= 1; }
                    if depth > 0 { j += 1; }
                }
                let cond_tokens = &self.tokens[self.pc+2..j];
                let cond_val = self.evaluate_expression(cond_tokens);
                
                let block_start = j + 2; // ) {
                
                if cond_val != 0 {
                    self.block_stack.push(BlockType::While(start_pc));
                    self.pc = block_start;
                } else {
                    let block_end = self.find_matching_brace(block_start - 1);
                    self.pc = block_end + 1;
                }
            } else if token == "}" {
                // End of block
                if let Some(block_type) = self.block_stack.pop() {
                    match block_type {
                        BlockType::While(start_pc) => {
                            self.pc = start_pc; // Loop back
                        },
                        BlockType::If => {
                            self.pc += 1;
                            // Check if next is else, if so skip it
                            if self.pc < self.tokens.len() && self.tokens[self.pc] == "else" {
                                if self.tokens[self.pc+1] == "{" {
                                    let block_end = self.find_matching_brace(self.pc + 1);
                                    self.pc = block_end + 1;
                                }
                            }
                        },
                        BlockType::Else => {
                            self.pc += 1;
                        },
                    }
                } else {
                    self.pc += 1;
                }
            } else if self.variables.contains_key(&token) {
                // Assignment: x = expr;
                if self.pc + 1 < self.tokens.len() && self.tokens[self.pc+1] == "=" {
                    let end = self.find_semicolon(self.pc);
                    let expr = &self.tokens[self.pc+2..end];
                    let val = self.evaluate_expression(expr);
                    self.variables.insert(token.clone(), val);
                    self.pc = end + 1;
                } else {
                    self.pc += 1;
                }
            } else {
                // Skip unknown
                self.pc += 1;
            }
        }
        (output, ExecutionState::Running)
    }
}

impl Interpreter for CInterpreter {
    fn execute(&mut self, command: &str) -> String {
        // For REPL, we just run it as a small program
        self.tokens = self.tokenize(command);
        self.pc = 0;
        self.block_stack.clear();
        let (output, _) = self.execute_steps(1000);
        output
    }

    fn run(&mut self, code: &str) -> String {
        self.variables.clear();
        self.tokens = self.tokenize(code);
        self.pc = 0;
        self.block_stack.clear();
        self.waiting_for_input_var = None;
        
        let mut output = String::new();
        loop {
            let (out, state) = self.execute_steps(1000);
            output.push_str(&out);
            if let ExecutionState::Finished = state {
                break;
            }
            if let ExecutionState::WaitingForInput(_) = state {
                // In run() mode (non-interactive), we can't really wait.
                // Just break or error?
                // For tests, we might want to mock input?
                // For now, just break to avoid infinite loop if input needed
                output.push_str("\n[Waiting for input]\n");
                break;
            }
        }
        output
    }

    fn start_execution(&mut self, code: &str) -> (String, ExecutionState) {
        self.variables.clear();
        self.tokens = self.tokenize(code);
        self.pc = 0;
        self.block_stack.clear();
        self.waiting_for_input_var = None;
        self.execute_steps(100) // Run a chunk
    }

    fn continue_execution(&mut self) -> (String, ExecutionState) {
        self.execute_steps(100)
    }

    fn step_execution(&mut self) -> (String, ExecutionState) {
        self.execute_steps(1)
    }

    fn provide_input(&mut self, input: &str) {
        if let Some(var) = self.waiting_for_input_var.take() {
            if let Ok(val) = input.trim().parse::<i32>() {
                self.variables.insert(var, val);
            }
        }
    }

    fn get_variables(&self) -> HashMap<String, String> {
        self.variables.iter()
            .map(|(k, v)| (k.clone(), v.to_string()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_c_printf() {
        let mut interpreter = CInterpreter::new();
        let code = "printf(\"Hello World\n\");";
        let output = interpreter.run(code);
        assert_eq!(output, "Hello World\n");
    }

    #[test]
    fn test_c_variables() {
        let mut interpreter = CInterpreter::new();
        let code = "int x = 10; printf(\"Val: %d\", x);";
        let output = interpreter.run(code);
        assert_eq!(output, "Val: 10");
    }

    #[test]
    fn test_c_if() {
        let mut interpreter = CInterpreter::new();
        let code = "int x = 5; if (x > 3) { printf(\"Yes\"); } else { printf(\"No\"); }";
        let output = interpreter.run(code);
        assert_eq!(output, "Yes");
        
        let code2 = "int x = 2; if (x > 3) { printf(\"Yes\"); } else { printf(\"No\"); }";
        let output2 = interpreter.run(code2);
        assert_eq!(output2, "No");
    }

    #[test]
    fn test_c_while() {
        let mut interpreter = CInterpreter::new();
        let code = "int i = 3; while (i > 0) { printf(\"%d \", i); i = i - 1; }";
        let output = interpreter.run(code);
        assert_eq!(output, "3 2 1 ");
    }

    #[test]
    fn test_c_scanf() {
        let mut interpreter = CInterpreter::new();
        let code = "int x; scanf(\"%d\", &x); printf(\"Val: %d\", x);";
        
        let (out1, state) = interpreter.start_execution(code);
        assert_eq!(out1, "");
        if let ExecutionState::WaitingForInput(var) = state {
            assert_eq!(var, "x");
            interpreter.provide_input("42");
            let (out2, state2) = interpreter.continue_execution();
            assert_eq!(out2, "Val: 42");
            assert_eq!(state2, ExecutionState::Finished);
        } else {
            panic!("Expected WaitingForInput");
        }
    }
}
