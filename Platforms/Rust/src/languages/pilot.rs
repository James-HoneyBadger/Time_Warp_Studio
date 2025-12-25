use crate::core::interpreter::{Interpreter, ExecutionState};
use std::collections::HashMap;

pub struct PilotInterpreter {
    last_match: bool,
    variables: HashMap<String, String>,
    accept_buffer: String,
    lines: Vec<String>,
    labels: HashMap<String, usize>,
    pc: usize,
    waiting_for_input_var: Option<String>,
}

impl PilotInterpreter {
    pub fn new() -> Self {
        Self {
            last_match: false,
            variables: HashMap::new(),
            accept_buffer: String::new(),
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
        self.last_match = false;
        self.accept_buffer.clear();
        self.variables.clear();

        for (i, line) in self.lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with('*') {
                let label = trimmed[1..].trim();
                self.labels.insert(label.to_string(), i);
            }
        }
    }
    
    fn replace_vars(&self, text: &str) -> String {
        let mut result = text.to_string();
        for (key, val) in &self.variables {
            result = result.replace(&format!("#{}", key), val);
        }
        result
    }

    fn execute_steps(&mut self, steps: usize) -> (String, ExecutionState) {
        let mut output = String::new();
        
        if self.waiting_for_input_var.is_some() {
             let var = self.waiting_for_input_var.clone().unwrap();
             return (output, ExecutionState::WaitingForInput(var));
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

            // T: Type
            if let Some(msg) = trimmed.strip_prefix("T:") {
                let msg = self.replace_vars(msg.trim());
                output.push_str(&format!("{}\n", msg));
            }
            // TY: Type Yes
            else if let Some(msg) = trimmed.strip_prefix("TY:") {
                if self.last_match {
                    let msg = self.replace_vars(msg.trim());
                    output.push_str(&format!("{}\n", msg));
                }
            }
            // TN: Type No
            else if let Some(msg) = trimmed.strip_prefix("TN:") {
                if !self.last_match {
                    let msg = self.replace_vars(msg.trim());
                    output.push_str(&format!("{}\n", msg));
                }
            }
            // R: Remark
            else if trimmed.starts_with("R:") {
                // Comment
            }
            // A: Accept
            else if let Some(args) = trimmed.strip_prefix("A:") {
                let args = args.trim();
                if !args.is_empty() && args.starts_with('#') {
                    let var = args[1..].to_string();
                    self.waiting_for_input_var = Some(var.clone());
                    self.pc += 1;
                    return (output, ExecutionState::WaitingForInput(var));
                } else {
                    // Just accept into buffer
                    self.waiting_for_input_var = Some("buffer".to_string()); // Special name
                    self.pc += 1;
                    return (output, ExecutionState::WaitingForInput("buffer".to_string()));
                }
            }
            // M: Match
            else if let Some(pattern) = trimmed.strip_prefix("M:") {
                let pat = pattern.trim().to_lowercase();
                let buffer = self.accept_buffer.to_lowercase();
                self.last_match = buffer.contains(&pat);
            }
            // J: Jump
            else if let Some(label) = trimmed.strip_prefix("J:") {
                let label = label.trim();
                if let Some(target_pc) = self.labels.get(label) {
                    self.pc = *target_pc;
                    continue;
                } else {
                    output.push_str(&format!("❌ Label not found: {}\n", label));
                }
            }
            // JY: Jump Yes
            else if let Some(label) = trimmed.strip_prefix("JY:") {
                if self.last_match {
                    let label = label.trim();
                    if let Some(target_pc) = self.labels.get(label) {
                        self.pc = *target_pc;
                        continue;
                    }
                }
            }
            // JN: Jump No
            else if let Some(label) = trimmed.strip_prefix("JN:") {
                if !self.last_match {
                    let label = label.trim();
                    if let Some(target_pc) = self.labels.get(label) {
                        self.pc = *target_pc;
                        continue;
                    }
                }
            }
            // E: End
            else if trimmed.starts_with("E:") {
                self.pc = self.lines.len();
                return (output, ExecutionState::Finished);
            }
            // *Label
            else if trimmed.starts_with('*') {
                // Label definition, skip
            }

            self.pc += 1;
        }
        
        (output, ExecutionState::Running)
    }
}

impl Interpreter for PilotInterpreter {
    fn execute(&mut self, command: &str) -> String {
        self.run(command)
    }

    fn run(&mut self, code: &str) -> String {
        let (mut output, mut state) = self.start_execution(code);
        loop {
            match state {
                ExecutionState::Finished => break,
                ExecutionState::WaitingForInput(_) => {
                    output.push_str("📝 Input requested. (Interactive input not supported in run())\n");
                    self.provide_input("0"); // Default input
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
            if var == "buffer" {
                self.accept_buffer = input.to_string();
            } else {
                self.variables.insert(var.clone(), input.to_string());
            }
            self.waiting_for_input_var = None;
        }
    }

    fn get_variables(&self) -> HashMap<String, String> {
        self.variables.clone()
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pilot_type() {
        let mut interpreter = PilotInterpreter::new();
        let code = "T: Hello";
        let output = interpreter.run(code);
        assert_eq!(output, "Hello\n");
    }

    #[test]
    fn test_pilot_jump() {
        let mut interpreter = PilotInterpreter::new();
        let code = "
T: Start
J: End
T: Skip
*End
T: Done
";
        let output = interpreter.run(code);
        assert_eq!(output, "Start\nDone\n");
    }

    #[test]
    fn test_pilot_match_jump() {
        let mut interpreter = PilotInterpreter::new();
        let code = "
A:
M: yes
TY: Yes matched
TN: No matched
A:
M: no
TY: Yes matched 2
TN: No matched 2
";
        let (mut output, mut state) = interpreter.start_execution(code);
        
        // First A:
        if let ExecutionState::WaitingForInput(_) = state {
            interpreter.provide_input("yes");
            let (out, s) = interpreter.continue_execution();
            output.push_str(&out);
            state = s;
        }
        
        // Second A:
        if let ExecutionState::WaitingForInput(_) = state {
            interpreter.provide_input("no");
            let (out, _) = interpreter.continue_execution();
            output.push_str(&out);
        }
        
        assert!(output.contains("Yes matched"));
        assert!(output.contains("Yes matched 2"));
    }

    #[test]
    fn test_pilot_conditional_jump() {
        let mut interpreter = PilotInterpreter::new();
        let code = "
A:
M: yes
JY: YesLabel
T: Should skip
*YesLabel
T: Jumped
";
        let (mut output, state) = interpreter.start_execution(code);
        
        if let ExecutionState::WaitingForInput(_) = state {
            interpreter.provide_input("yes");
            let (out, _) = interpreter.continue_execution();
            output.push_str(&out);
        }

        assert!(output.contains("Jumped"));
        assert!(!output.contains("Should skip"));
    }
}

#[cfg(test)]
mod more_tests {
    use super::*;

    #[test]
    fn test_pilot_accept() {
        let mut interpreter = PilotInterpreter::new();
        let code = "A:";
        let output = interpreter.run(code);
        assert!(output.contains("Input requested"));
    }
}
