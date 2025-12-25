use crate::core::interpreter::{Interpreter, ExecutionState};
use std::collections::HashMap;

pub struct BasicInterpreter {
    variables: HashMap<String, f64>,
    lines: Vec<String>,
    labels: HashMap<i32, usize>,
    pc: usize,
    waiting_for_input_var: Option<String>,
}

impl BasicInterpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            lines: Vec::new(),
            labels: HashMap::new(),
            pc: 0,
            waiting_for_input_var: None,
        }
    }

    fn parse_program(&mut self, code: &str) {
        self.lines = code.lines().map(|s| s.to_string()).collect();
        self.labels.clear();
        self.pc = 0;
        self.waiting_for_input_var = None;

        for (i, line) in self.lines.iter().enumerate() {
            let trimmed = line.trim();
            if let Some((label, _)) = trimmed.split_once(' ') {
                if let Ok(line_num) = label.parse::<i32>() {
                    self.labels.insert(line_num, i);
                }
            }
        }
    }

    fn evaluate_expression(&self, expr: &str) -> f64 {
        let expr = expr.trim();
        
        // Simple binary expression parser (e.g. "A + 1")
        // Supports +, -, *, /
        let operators = ['+', '-', '*', '/'];
        
        for op in operators {
            if let Some(idx) = expr.find(op) {
                let left = &expr[..idx];
                let right = &expr[idx+1..];
                let l_val = self.evaluate_term(left);
                let r_val = self.evaluate_term(right);
                
                return match op {
                    '+' => l_val + r_val,
                    '-' => l_val - r_val,
                    '*' => l_val * r_val,
                    '/' => if r_val != 0.0 { l_val / r_val } else { 0.0 },
                    _ => 0.0,
                };
            }
        }
        
        self.evaluate_term(expr)
    }

    fn evaluate_term(&self, term: &str) -> f64 {
        let term = term.trim();
        if let Ok(val) = term.parse::<f64>() {
            return val;
        }
        if let Some(val) = self.variables.get(term) {
            return *val;
        }
        0.0
    }

    fn execute_steps(&mut self, steps: usize) -> (String, ExecutionState) {
        let mut output = String::new();
        
        if self.waiting_for_input_var.is_some() {
             return (output, ExecutionState::WaitingForInput(self.waiting_for_input_var.clone().unwrap()));
        }

        for _ in 0..steps {
            if self.pc >= self.lines.len() {
                return (output, ExecutionState::Finished);
            }

            let line = self.lines[self.pc].clone();
            let trimmed = line.trim();
            
            if trimmed.is_empty() {
                self.pc += 1;
                continue;
            }

            let cmd_part = if let Some((label, rest)) = trimmed.split_once(' ') {
                if label.parse::<i32>().is_ok() {
                    rest.trim()
                } else {
                    trimmed
                }
            } else {
                trimmed
            };

            let upper_cmd = cmd_part.to_uppercase();

            if upper_cmd.starts_with("PRINT ") {
                let content = cmd_part[6..].trim();
                if content.starts_with('"') && content.ends_with('"') {
                    output.push_str(&format!("{}\n", &content[1..content.len()-1]));
                } else {
                    let val = self.evaluate_expression(content);
                    output.push_str(&format!("{}\n", val));
                }
            } else if upper_cmd.starts_with("GOTO ") {
                let target = cmd_part[5..].trim();
                if let Ok(target_line) = target.parse::<i32>() {
                    if let Some(target_pc) = self.labels.get(&target_line) {
                        self.pc = *target_pc;
                        continue;
                    } else {
                        output.push_str(&format!("❌ Label not found: {}\n", target));
                    }
                }
            } else if upper_cmd.starts_with("IF ") {
                let parts: Vec<&str> = cmd_part.split_whitespace().collect();
                if parts.len() >= 6 && parts[4] == "THEN" {
                    let var = parts[1];
                    let op = parts[2];
                    let val_str = parts[3];
                    let target_str = parts[5];

                    let var_val = self.evaluate_expression(var);
                    let cmp_val = self.evaluate_expression(val_str);
                    
                    let condition = match op {
                        ">" => var_val > cmp_val,
                        "<" => var_val < cmp_val,
                        "=" => (var_val - cmp_val).abs() < f64::EPSILON,
                        _ => false,
                    };

                    if condition {
                        if let Ok(target_line) = target_str.parse::<i32>() {
                            if let Some(target_pc) = self.labels.get(&target_line) {
                                self.pc = *target_pc;
                                continue;
                            }
                        }
                    }
                }
            } else if upper_cmd.starts_with("INPUT ") {
                let var = cmd_part[6..].trim().to_string();
                self.waiting_for_input_var = Some(var.clone());
                self.pc += 1; 
                return (output, ExecutionState::WaitingForInput(var));
            } else if let Some((var, val)) = cmd_part.split_once('=') {
                let var = var.trim().to_string();
                let val = self.evaluate_expression(val.trim());
                self.variables.insert(var, val);
            } else if upper_cmd == "END" {
                self.pc = self.lines.len();
                return (output, ExecutionState::Finished);
            }

            self.pc += 1;
        }
        
        (output, ExecutionState::Running)
    }
}

impl Interpreter for BasicInterpreter {
    fn execute(&mut self, command: &str) -> String {
        self.run(command)
    }

    fn run(&mut self, code: &str) -> String {
        let (mut output, mut state) = self.start_execution(code);
        loop {
            match state {
                ExecutionState::Finished => break,
                ExecutionState::WaitingForInput(var) => {
                    output.push_str(&format!("📝 Input requested for '{}'. (Interactive input not supported in run(), setting to 0)\n", var));
                    self.provide_input("0");
                    let (out, new_state) = self.continue_execution();
                    output.push_str(&out);
                    state = new_state;
                }
                ExecutionState::Running => {
                    let (out, new_state) = self.continue_execution();
                    output.push_str(&out);
                    state = new_state;
                }
            }
        }
        output
    }

    fn start_execution(&mut self, code: &str) -> (String, ExecutionState) {
        self.parse_program(code);
        self.continue_execution()
    }

    fn continue_execution(&mut self) -> (String, ExecutionState) {
        self.execute_steps(100)
    }

    fn step_execution(&mut self) -> (String, ExecutionState) {
        self.execute_steps(1)
    }

    fn load_code(&mut self, code: &str) {
        self.parse_program(code);
    }

    fn provide_input(&mut self, input: &str) {
        if let Some(var) = &self.waiting_for_input_var {
            let val = input.trim().parse::<f64>().unwrap_or(0.0);
            self.variables.insert(var.clone(), val);
            self.waiting_for_input_var = None;
        }
    }

    fn get_variables(&self) -> HashMap<String, String> {
        self.variables.iter().map(|(k, v)| (k.clone(), v.to_string())).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_print() {
        let mut interpreter = BasicInterpreter::new();
        let code = "10 PRINT \"Hello\"";
        let output = interpreter.run(code);
        assert_eq!(output, "Hello\n");
    }

    #[test]
    fn test_basic_goto() {
        let mut interpreter = BasicInterpreter::new();
        let code = "
10 PRINT \"Start\"
20 GOTO 40
30 PRINT \"Skip\"
40 PRINT \"End\"
";
        let output = interpreter.run(code);
        assert_eq!(output, "Start\nEnd\n");
    }

    #[test]
    fn test_basic_if() {
        let mut interpreter = BasicInterpreter::new();
        let code = "
10 X = 10
20 IF X > 5 THEN 40
30 PRINT \"No\"
40 PRINT \"Yes\"
";
        let output = interpreter.run(code);
        assert_eq!(output, "Yes\n");
    }

    #[test]
    fn test_basic_loop() {
        let mut interpreter = BasicInterpreter::new();
        let code = "
10 I = 1
20 IF I > 3 THEN 60
30 PRINT I
40 I = I + 1
50 GOTO 20
60 PRINT \"Done\"
";
        let output = interpreter.run(code);
        assert_eq!(output, "1\n2\n3\nDone\n");
    }
}

    #[test]
    fn test_basic_input() {
        let mut interpreter = BasicInterpreter::new();
        let code = "10 INPUT X\n20 PRINT X";
        let output = interpreter.run(code);
        assert!(output.contains("Input requested"));
        assert!(output.contains("0"));
    }
