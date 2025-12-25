#[derive(Clone, Debug)]
pub enum DrawCommand {
    Clear { color: (u8, u8, u8) },
    Line { x1: f32, y1: f32, x2: f32, y2: f32, color: (u8, u8, u8), width: f32 },
    Turtle { x: f32, y: f32, angle: f32, visible: bool, color: (u8, u8, u8) },
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExecutionState {
    Running,
    WaitingForInput(String),
    Finished,
}

pub trait Interpreter {
    fn execute(&mut self, command: &str) -> String;
    
    fn run(&mut self, code: &str) -> String {
        let mut output = String::new();
        for line in code.lines() {
            output.push_str(&self.execute(line));
        }
        output
    }

    fn start_execution(&mut self, code: &str) -> (String, ExecutionState) {
        let output = self.run(code);
        (output, ExecutionState::Finished)
    }

    fn continue_execution(&mut self) -> (String, ExecutionState) {
        (String::new(), ExecutionState::Finished)
    }

    fn step_execution(&mut self) -> (String, ExecutionState) {
        self.continue_execution()
    }

    fn load_code(&mut self, _code: &str) {}

    fn provide_input(&mut self, _input: &str) {}

    fn get_draw_commands(&self) -> Vec<DrawCommand> {
        Vec::new()
    }

    fn get_variables(&self) -> std::collections::HashMap<String, String> {
        std::collections::HashMap::new()
    }
}
