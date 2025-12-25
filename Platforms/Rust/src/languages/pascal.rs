use crate::core::interpreter::{Interpreter, ExecutionState};
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum BlockType {
    For { var: String, end: i32, start_pc: usize },
    If,
    Block, // BEGIN ... END
}

pub struct PascalInterpreter {
    variables: HashMap<String, i32>,
    output_buffer: String,
    tokens: Vec<String>,
    pc: usize,
    block_stack: Vec<BlockType>,
    waiting_for_input_var: Option<String>,
}

impl PascalInterpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            output_buffer: String::new(),
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
            } else if c == '\'' {
                // String literal
                chars.next(); // consume opening '
                let mut s = String::new();
                s.push('\''); // keep quotes for identification
                while let Some(&nc) = chars.peek() {
                    if nc == '\'' {
                        chars.next();
                        s.push('\'');
                        break;
                    }
                    s.push(chars.next().unwrap());
                }
                tokens.push(s);
            } else if c.is_alphabetic() || c == '_' {
                // Identifier or keyword
                let mut s = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_alphanumeric() || nc == '_' {
                        s.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                tokens.push(s);
            } else if c.is_numeric() {
                // Number
                let mut s = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_numeric() {
                        s.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                tokens.push(s);
            } else {
                // Symbols
                let mut s = String::new();
                s.push(chars.next().unwrap());
                
                // Check for 2-char symbols: :=, <=, >=, <>
                if let Some(&nc) = chars.peek() {
                    let two_char = format!("{}{}", s, nc);
                    if two_char == ":=" || two_char == "<=" || two_char == ">=" || two_char == "<>" {
                        s.push(chars.next().unwrap());
                    }
                }
                tokens.push(s);
            }
        }
        tokens
    }

    fn evaluate_expression(&self, tokens: &[String]) -> i32 {
        // Very simple evaluator: just handles single numbers or variables for now
        // Or simple binary ops if we want to get fancy, but let's stick to basics first.
        // If token is string literal, we can't return i32. 
        // This function assumes integer expression.
        
        if tokens.is_empty() { return 0; }
        
        // Handle simple binary: A + B
        if tokens.len() == 3 {
            let op = &tokens[1];
            let left = self.get_value(&tokens[0]);
            let right = self.get_value(&tokens[2]);
            match op.as_str() {
                "+" => return left + right,
                "-" => return left - right,
                "*" => return left * right,
                "/" => return if right != 0 { left / right } else { 0 },
                ">" => return if left > right { 1 } else { 0 },
                "<" => return if left < right { 1 } else { 0 },
                "=" => return if left == right { 1 } else { 0 },
                _ => {}
            }
        }

        self.get_value(&tokens[0])
    }

    fn get_value(&self, token: &str) -> i32 {
        if let Ok(val) = token.parse::<i32>() {
            val
        } else if let Some(val) = self.variables.get(token) {
            *val
        } else {
            0
        }
    }

    fn get_string_content(&self, token: &str) -> String {
        if token.starts_with('\'') && token.ends_with('\'') {
            token[1..token.len()-1].to_string()
        } else {
            // Try variable
            if let Some(val) = self.variables.get(token) {
                val.to_string()
            } else {
                token.to_string()
            }
        }
    }

    fn find_matching_end(&self, start: usize) -> usize {
        let mut depth = 1;
        let mut i = start + 1;
        while i < self.tokens.len() && depth > 0 {
            let t = self.tokens[i].to_uppercase();
            if t == "BEGIN" { depth += 1; }
            if t == "END" { depth -= 1; }
            if depth > 0 { i += 1; }
        }
        i
    }

    fn find_statement_end(&self, start: usize) -> usize {
        if start >= self.tokens.len() { return start; }
        if self.tokens[start].to_uppercase() == "BEGIN" {
            let end = self.find_matching_end(start);
            // Check for ; after END
            if end + 1 < self.tokens.len() && self.tokens[end+1] == ";" {
                end + 1
            } else {
                end
            }
        } else {
            let mut i = start;
            while i < self.tokens.len() && self.tokens[i] != ";" {
                i += 1;
            }
            i
        }
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
            let upper = token.to_uppercase();

            match upper.as_str() {
                "PROGRAM" => {
                    // PROGRAM Name; -> skip to ;
                    let end = self.find_statement_end(self.pc + 1); // Actually just find ;
                    self.pc = end + 1;
                },
                "BEGIN" => {
                    self.block_stack.push(BlockType::Block);
                    self.pc += 1;
                },
                "END" | "END." => {
                    // End of block
                    if let Some(block_type) = self.block_stack.pop() {
                        match block_type {
                            BlockType::For { var, end, start_pc } => {
                                // Increment var
                                let current = *self.variables.get(&var).unwrap_or(&0);
                                if current < end {
                                    self.variables.insert(var.clone(), current + 1);
                                    self.block_stack.push(BlockType::For { var, end, start_pc });
                                    self.pc = start_pc; // Loop back
                                } else {
                                    self.pc += 1;
                                    if self.pc < self.tokens.len() && self.tokens[self.pc] == ";" { self.pc += 1; }
                                    if self.pc < self.tokens.len() && self.tokens[self.pc] == "." { self.pc += 1; }
                                }
                            },
                            _ => {
                                self.pc += 1;
                                if self.pc < self.tokens.len() && self.tokens[self.pc] == ";" { self.pc += 1; }
                                if self.pc < self.tokens.len() && self.tokens[self.pc] == "." { self.pc += 1; }
                            }
                        }
                    } else {
                        self.pc += 1;
                        if self.pc < self.tokens.len() && self.tokens[self.pc] == "." { self.pc += 1; }
                    }
                },
                "WRITELN" | "WRITE" => {
                    let is_ln = upper == "WRITELN";
                    if self.pc + 1 < self.tokens.len() && self.tokens[self.pc+1] == "(" {
                        // Parse args until )
                        let mut j = self.pc + 2;
                        let mut content = String::new();
                        while j < self.tokens.len() && self.tokens[j] != ")" {
                            if self.tokens[j] != "," {
                                content.push_str(&self.get_string_content(&self.tokens[j]));
                            }
                            j += 1;
                        }
                        if is_ln {
                            output.push_str(&format!("{}{}\n", self.output_buffer, content));
                            self.output_buffer.clear();
                        } else {
                            self.output_buffer.push_str(&content);
                        }
                        
                        self.pc = j + 1; // consume )
                        if self.pc < self.tokens.len() && self.tokens[self.pc] == ";" { self.pc += 1; }
                    } else {
                        // Empty WRITELN
                        if is_ln { output.push_str("\n"); }
                        self.pc += 1;
                        if self.pc < self.tokens.len() && self.tokens[self.pc] == ";" { self.pc += 1; }
                    }
                },
                "READLN" | "READ" => {
                    // READLN(Var);
                    if self.pc + 1 < self.tokens.len() && self.tokens[self.pc+1] == "(" {
                        let var_name = self.tokens[self.pc+2].clone();
                        // Assume single var for now
                        self.waiting_for_input_var = Some(var_name.clone());
                        
                        // Advance PC so when we resume we are past this instruction
                        let mut j = self.pc + 2;
                        while j < self.tokens.len() && self.tokens[j] != ")" { j += 1; }
                        self.pc = j + 1;
                        if self.pc < self.tokens.len() && self.tokens[self.pc] == ";" { self.pc += 1; }
                        
                        return (output, ExecutionState::WaitingForInput(var_name));
                    } else {
                        self.pc += 1;
                    }
                },
                "FOR" => {
                    // FOR I := 1 TO 10 DO stmt
                    if self.pc + 7 < self.tokens.len() {
                        let var_name = self.tokens[self.pc+1].clone();
                        let start_val = self.get_value(&self.tokens[self.pc+3]);
                        let end_val = self.get_value(&self.tokens[self.pc+5]);
                        
                        self.variables.insert(var_name.clone(), start_val);
                        
                        let body_start = self.pc + 7;
                        
                        if start_val <= end_val {
                            self.block_stack.push(BlockType::For { var: var_name, end: end_val, start_pc: body_start });
                            self.pc = body_start;
                        } else {
                            // Skip body
                            let body_end = self.find_statement_end(body_start);
                            self.pc = body_end + 1;
                        }
                    } else {
                        self.pc += 1;
                    }
                },
                "IF" => {
                    // IF cond THEN stmt
                    if self.pc + 4 < self.tokens.len() {
                        let lhs = &self.tokens[self.pc+1];
                        let op = &self.tokens[self.pc+2];
                        let rhs = &self.tokens[self.pc+3];
                        
                        let l_val = self.get_value(lhs);
                        let r_val = self.get_value(rhs);
                        let condition = match op.as_str() {
                            ">" => l_val > r_val,
                            "<" => l_val < r_val,
                            "=" => l_val == r_val,
                            "<>" => l_val != r_val,
                            _ => false,
                        };
                        
                        let body_start = self.pc + 5;
                        
                        if condition {
                            self.block_stack.push(BlockType::If);
                            self.pc = body_start;
                        } else {
                            let body_end = self.find_statement_end(body_start);
                            self.pc = body_end + 1;
                        }
                    } else {
                        self.pc += 1;
                    }
                },
                ";" => {
                    self.pc += 1;
                },
                _ => {
                    // Assignment? VAR := VAL;
                    if self.pc + 2 < self.tokens.len() && self.tokens[self.pc+1] == ":=" {
                        let var_name = token.clone();
                        let mut expr_end = self.pc + 2;
                        while expr_end < self.tokens.len() && self.tokens[expr_end] != ";" {
                            expr_end += 1;
                        }
                        
                        let expr_tokens = &self.tokens[self.pc+2..expr_end];
                        let val = self.evaluate_expression(expr_tokens);
                        self.variables.insert(var_name, val);
                        
                        self.pc = expr_end;
                        if self.pc < self.tokens.len() && self.tokens[self.pc] == ";" { self.pc += 1; }
                    } else {
                        self.pc += 1;
                    }
                }
            }
        }
        (output, ExecutionState::Running)
    }
}

impl Interpreter for PascalInterpreter {
    fn execute(&mut self, command: &str) -> String {
        self.tokens = self.tokenize(command);
        self.pc = 0;
        self.block_stack.clear();
        let (output, _) = self.execute_steps(1000);
        output
    }

    fn run(&mut self, code: &str) -> String {
        self.variables.clear();
        self.output_buffer.clear();
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
                output.push_str("\n[Waiting for input]\n");
                break;
            }
        }
        output
    }

    fn start_execution(&mut self, code: &str) -> (String, ExecutionState) {
        self.variables.clear();
        self.output_buffer.clear();
        self.tokens = self.tokenize(code);
        self.pc = 0;
        self.block_stack.clear();
        self.waiting_for_input_var = None;
        self.execute_steps(100)
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
    fn test_pascal_writeln() {
        let mut interpreter = PascalInterpreter::new();
        let code = "PROGRAM Test; BEGIN WRITELN('Hello World'); END.";
        let output = interpreter.run(code);
        assert_eq!(output, "Hello World\n");
    }

    #[test]
    fn test_pascal_variables() {
        let mut interpreter = PascalInterpreter::new();
        let code = "PROGRAM Test; VAR X; BEGIN X := 10; WRITELN(X); END.";
        let output = interpreter.run(code);
        assert_eq!(output, "10\n");
    }

    #[test]
    fn test_pascal_for() {
        let mut interpreter = PascalInterpreter::new();
        let code = "PROGRAM Test; VAR I; BEGIN FOR I := 1 TO 3 DO WRITELN(I); END.";
        let output = interpreter.run(code);
        assert_eq!(output, "1\n2\n3\n");
    }

    #[test]
    fn test_pascal_if() {
        let mut interpreter = PascalInterpreter::new();
        let code = "PROGRAM Test; VAR X; BEGIN X := 5; IF X > 3 THEN WRITELN('Yes'); END.";
        let output = interpreter.run(code);
        assert_eq!(output, "Yes\n");
    }

    #[test]
    fn test_pascal_readln() {
        let mut interpreter = PascalInterpreter::new();
        let code = "PROGRAM Test; VAR X; BEGIN READLN(X); WRITELN(X); END.";
        
        let (out1, state) = interpreter.start_execution(code);
        assert_eq!(out1, "");
        if let ExecutionState::WaitingForInput(var) = state {
            assert_eq!(var, "X");
            interpreter.provide_input("42");
            let (out2, state2) = interpreter.continue_execution();
            assert_eq!(out2, "42\n");
            assert_eq!(state2, ExecutionState::Finished);
        } else {
            panic!("Expected WaitingForInput");
        }
    }
}
