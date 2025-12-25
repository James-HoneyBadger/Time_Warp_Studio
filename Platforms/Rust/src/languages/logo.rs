use crate::core::interpreter::{Interpreter, DrawCommand, ExecutionState};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct Procedure {
    params: Vec<String>,
    body: Vec<String>,
}

#[derive(Clone, Debug)]
struct LogoFrame {
    tokens: Vec<String>,
    pc: usize,
    repeat_count: Option<i32>, // If set, this frame is a REPEAT block
    is_procedure: bool,        // If true, popping this frame pops a scope
}

pub struct LogoInterpreter {
    turtle_x: f32,
    turtle_y: f32,
    angle: f32,
    pen_down: bool,
    pen_color: (u8, u8, u8),
    pen_width: f32,
    draw_commands: Vec<DrawCommand>,
    procedures: HashMap<String, Procedure>,
    scopes: Vec<HashMap<String, f32>>,
    
    // Execution State
    call_stack: Vec<LogoFrame>,
    state: ExecutionState,
    output_buffer: String,
}

impl LogoInterpreter {
    pub fn new() -> Self {
        Self {
            turtle_x: 0.0,
            turtle_y: 0.0,
            angle: 0.0,
            pen_down: true,
            pen_color: (255, 255, 255),
            pen_width: 1.0,
            draw_commands: Vec::new(),
            procedures: HashMap::new(),
            scopes: vec![HashMap::new()], // Global scope
            call_stack: Vec::new(),
            state: ExecutionState::Finished,
            output_buffer: String::new(),
        }
    }

    #[allow(dead_code)]
    pub fn get_turtle_state(&self) -> (f32, f32, f32) {
        (self.turtle_x, self.turtle_y, self.angle)
    }

    fn get_var(scopes: &[HashMap<String, f32>], name: &str) -> f32 {
        for scope in scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return *val;
            }
        }
        0.0
    }

    fn tokenize(&self, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut current = String::new();
        
        for c in input.chars() {
            if "[]()+-*/=<>&|".contains(c) {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
                tokens.push(c.to_string());
            } else if c.is_whitespace() {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
            } else {
                current.push(c);
            }
        }
        if !current.is_empty() {
            tokens.push(current);
        }
        tokens
    }

    fn eval_expr(scopes: &[HashMap<String, f32>], tokens: &[String], idx: &mut usize) -> f32 {
        if *idx >= tokens.len() { return 0.0; }
        let mut val = Self::eval_term(scopes, tokens, idx);
        while *idx < tokens.len() {
            let op = &tokens[*idx];
            if op == "+" {
                *idx += 1;
                val += Self::eval_term(scopes, tokens, idx);
            } else if op == "-" {
                *idx += 1;
                val -= Self::eval_term(scopes, tokens, idx);
            } else {
                break;
            }
        }
        val
    }

    fn eval_term(scopes: &[HashMap<String, f32>], tokens: &[String], idx: &mut usize) -> f32 {
        if *idx >= tokens.len() { return 0.0; }
        let mut val = Self::eval_factor(scopes, tokens, idx);
        while *idx < tokens.len() {
            let op = &tokens[*idx];
            if op == "*" {
                *idx += 1;
                val *= Self::eval_factor(scopes, tokens, idx);
            } else if op == "/" {
                *idx += 1;
                let div = Self::eval_factor(scopes, tokens, idx);
                if div != 0.0 { val /= div; }
            } else {
                break;
            }
        }
        val
    }

    fn eval_factor(scopes: &[HashMap<String, f32>], tokens: &[String], idx: &mut usize) -> f32 {
        if *idx >= tokens.len() { return 0.0; }
        let token = &tokens[*idx];
        
        if let Ok(v) = token.parse::<f32>() {
            *idx += 1;
            v
        } else if token.starts_with(':') {
            *idx += 1;
            Self::get_var(scopes, &token[1..])
        } else if token == "(" {
            *idx += 1;
            let v = Self::eval_expr(scopes, tokens, idx);
            if *idx < tokens.len() && tokens[*idx] == ")" {
                *idx += 1;
            }
            v
        } else if token == "-" {
            *idx += 1;
            -Self::eval_factor(scopes, tokens, idx)
        } else {
            // Not a factor, don't consume
            0.0
        }
    }

    fn eval_condition(scopes: &[HashMap<String, f32>], tokens: &[String], idx: &mut usize) -> bool {
        let left = Self::eval_expr(scopes, tokens, idx);
        if *idx >= tokens.len() { return false; }
        let op = &tokens[*idx];
        if op == "<" || op == ">" || op == "=" {
            *idx += 1;
            let right = Self::eval_expr(scopes, tokens, idx);
            match op.as_str() {
                "<" => left < right,
                ">" => left > right,
                "=" => (left - right).abs() < 0.001,
                _ => false
            }
        } else {
            false
        }
    }

    fn execute_step(&mut self) {
        if self.call_stack.is_empty() {
            self.state = ExecutionState::Finished;
            return;
        }

        let frame_idx = self.call_stack.len() - 1;
        
        // Check if frame is finished
        let is_finished = {
            let frame = &self.call_stack[frame_idx];
            frame.pc >= frame.tokens.len()
        };

        if is_finished {
            let frame = &mut self.call_stack[frame_idx];
            if let Some(count) = frame.repeat_count {
                if count > 1 {
                    frame.repeat_count = Some(count - 1);
                    frame.pc = 0;
                    return;
                }
            }
            
            let is_proc = frame.is_procedure;
            self.call_stack.pop();
            if is_proc {
                self.scopes.pop();
            }
            
            if self.call_stack.is_empty() {
                self.state = ExecutionState::Finished;
            }
            return;
        }

        // Read Phase
        enum Action {
            None,
            Move(f32),
            Turn(f32),
            Pen(bool),
            Color((u8, u8, u8)),
            Width(f32),
            Clear,
            Home,
            Print(String),
            Repeat(i32, Vec<String>),
            Call(String, Vec<f32>),
            Define(String, Procedure),
            Stop,
            If(Vec<String>),
            Error(String),
        }

        let (action, new_pc) = {
            let frame = &self.call_stack[frame_idx];
            let tokens = &frame.tokens;
            let mut pc = frame.pc;
            
            let token = &tokens[pc];
            pc += 1;
            let upper = token.to_uppercase();
            
            let action = match upper.as_str() {
                "FD" | "FORWARD" => {
                    let dist = Self::eval_expr(&self.scopes, tokens, &mut pc);
                    Action::Move(dist)
                },
                "BK" | "BACK" => {
                    let dist = Self::eval_expr(&self.scopes, tokens, &mut pc);
                    Action::Move(-dist)
                },
                "RT" | "RIGHT" => {
                    let angle = Self::eval_expr(&self.scopes, tokens, &mut pc);
                    Action::Turn(angle)
                },
                "LT" | "LEFT" => {
                    let angle = Self::eval_expr(&self.scopes, tokens, &mut pc);
                    Action::Turn(-angle)
                },
                "PU" | "PENUP" => Action::Pen(false),
                "PD" | "PENDOWN" => Action::Pen(true),
                "SETPENWIDTH" | "SETWIDTH" | "SETSIZE" => {
                    let width = Self::eval_expr(&self.scopes, tokens, &mut pc);
                    Action::Width(width)
                },
                "CS" | "CLEAR" => Action::Clear,
                "HOME" => Action::Home,
                "STOP" => Action::Stop,
                "SETPC" | "SETPENCOLOR" => {
                    if pc < tokens.len() {
                        if tokens[pc] == "[" {
                            pc += 1;
                            let r = Self::eval_expr(&self.scopes, tokens, &mut pc) as u8;
                            let g = Self::eval_expr(&self.scopes, tokens, &mut pc) as u8;
                            let b = Self::eval_expr(&self.scopes, tokens, &mut pc) as u8;
                            if pc < tokens.len() && tokens[pc] == "]" {
                                pc += 1;
                            }
                            Action::Color((r, g, b))
                        } else if tokens[pc].starts_with('"') {
                            // Handle string color names like "RED"
                            let color_name = tokens[pc][1..].to_uppercase();
                            pc += 1;
                            let color = match color_name.as_str() {
                                "BLACK" => (0, 0, 0),
                                "BLUE" => (0, 0, 255),
                                "GREEN" => (0, 255, 0),
                                "CYAN" => (0, 255, 255),
                                "RED" => (255, 0, 0),
                                "MAGENTA" => (255, 0, 255),
                                "YELLOW" => (255, 255, 0),
                                "WHITE" => (255, 255, 255),
                                "BROWN" => (165, 42, 42),
                                "TAN" => (210, 180, 140),
                                "FOREST" => (34, 139, 34),
                                "AQUA" => (127, 255, 212),
                                "SALMON" => (250, 128, 114),
                                "PURPLE" => (128, 0, 128),
                                "ORANGE" => (255, 165, 0),
                                "GREY" | "GRAY" => (128, 128, 128),
                                "PINK" => (255, 192, 203),
                                _ => (255, 255, 255),
                            };
                            Action::Color(color)
                        } else {
                            let idx = Self::eval_expr(&self.scopes, tokens, &mut pc) as usize;
                            let color = match idx {
                                0 => (0, 0, 0), 1 => (0, 0, 255), 2 => (0, 255, 0), 3 => (0, 255, 255),
                                4 => (255, 0, 0), 5 => (255, 0, 255), 6 => (255, 255, 0), 7 => (255, 255, 255),
                                8 => (165, 42, 42), 9 => (210, 180, 140), 10 => (34, 139, 34), 11 => (127, 255, 212),
                                12 => (250, 128, 114), 13 => (128, 0, 128), 14 => (255, 165, 0), 15 => (128, 128, 128),
                                _ => (255, 255, 255),
                            };
                            Action::Color(color)
                        }
                    } else {
                        Action::None
                    }
                },
                "PRINT" | "PR" => {
                    if pc < tokens.len() {
                        let content = &tokens[pc];
                        if content.starts_with('"') {
                             let msg = content[1..].to_string();
                             pc += 1;
                             Action::Print(msg)
                        } else if content == "[" {
                             let mut depth = 1;
                             let mut j = pc + 1;
                             let mut msg = String::new();
                             while j < tokens.len() {
                                 if tokens[j] == "[" { depth += 1; }
                                 if tokens[j] == "]" { depth -= 1; }
                                 if depth == 0 { break; }
                                 msg.push_str(&tokens[j]);
                                 msg.push(' ');
                                 j += 1;
                             }
                             pc = j + 1;
                             Action::Print(msg.trim().to_string())
                        } else {
                             let val = Self::eval_expr(&self.scopes, tokens, &mut pc);
                             Action::Print(val.to_string())
                        }
                    } else {
                        Action::None
                    }
                },
                "REPEAT" => {
                    let count = Self::eval_expr(&self.scopes, tokens, &mut pc) as i32;
                    if pc < tokens.len() && tokens[pc] == "[" {
                        let mut depth = 1;
                        let start = pc + 1;
                        let mut end = start;
                        pc += 1;
                        while pc < tokens.len() {
                            if tokens[pc] == "[" { depth += 1; }
                            if tokens[pc] == "]" { depth -= 1; }
                            if depth == 0 {
                                end = pc;
                                pc += 1;
                                break;
                            }
                            pc += 1;
                        }
                        if depth == 0 {
                            Action::Repeat(count, tokens[start..end].to_vec())
                        } else {
                            Action::None
                        }
                    } else {
                        Action::None
                    }
                },
                "IF" => {
                    let cond = Self::eval_condition(&self.scopes, tokens, &mut pc);
                    if cond {
                        if pc < tokens.len() && tokens[pc] == "[" {
                            let mut depth = 1;
                            let start = pc + 1;
                            let mut end = start;
                            pc += 1;
                            while pc < tokens.len() {
                                if tokens[pc] == "[" { depth += 1; }
                                if tokens[pc] == "]" { depth -= 1; }
                                if depth == 0 {
                                    end = pc;
                                    pc += 1;
                                    break;
                                }
                                pc += 1;
                            }
                            if depth == 0 {
                                Action::If(tokens[start..end].to_vec())
                            } else {
                                Action::None
                            }
                        } else {
                            Action::None
                        }
                    } else {
                        // Skip block
                        if pc < tokens.len() && tokens[pc] == "[" {
                            let mut depth = 1;
                            pc += 1;
                            while pc < tokens.len() {
                                if tokens[pc] == "[" { depth += 1; }
                                if tokens[pc] == "]" { depth -= 1; }
                                if depth == 0 {
                                    pc += 1;
                                    break;
                                }
                                pc += 1;
                            }
                        }
                        Action::None
                    }
                },
                "TO" => {
                    if pc < tokens.len() {
                        let name = tokens[pc].to_uppercase();
                        pc += 1;
                        let mut params = Vec::new();
                        while pc < tokens.len() && tokens[pc].starts_with(':') {
                            params.push(tokens[pc][1..].to_string());
                            pc += 1;
                        }
                        
                        let mut body = Vec::new();
                        while pc < tokens.len() {
                            if tokens[pc].to_uppercase() == "END" {
                                pc += 1;
                                break;
                            }
                            body.push(tokens[pc].clone());
                            pc += 1;
                        }
                        Action::Define(name, Procedure { params, body })
                    } else {
                        Action::None
                    }
                },
                "]" => Action::None,
                _ => {
                    if let Some(proc) = self.procedures.get(&upper) {
                        let mut args = Vec::new();
                        for _ in 0..proc.params.len() {
                            args.push(Self::eval_expr(&self.scopes, tokens, &mut pc));
                        }
                        Action::Call(upper, args)
                    } else {
                        Action::Error(format!("Unknown command: {}", upper))
                    }
                }
            };
            (action, pc)
        };

        // Write Phase
        self.call_stack[frame_idx].pc = new_pc;

        match action {
            Action::Move(dist) => {
                let rad = self.angle.to_radians();
                let dx = rad.sin() * dist;
                let dy = rad.cos() * dist;
                let new_x = self.turtle_x + dx;
                let new_y = self.turtle_y + dy;
                
                if self.pen_down {
                    self.draw_commands.push(DrawCommand::Line {
                        x1: self.turtle_x,
                        y1: self.turtle_y,
                        x2: new_x,
                        y2: new_y,
                        color: self.pen_color,
                        width: self.pen_width,
                    });
                }
                self.turtle_x = new_x;
                self.turtle_y = new_y;
            },
            Action::Turn(angle) => {
                self.angle = (self.angle + angle) % 360.0;
            },
            Action::Pen(down) => {
                self.pen_down = down;
                self.output_buffer.push_str(if down { "🐢 PEN DOWN\n" } else { "🐢 PEN UP\n" });
            },
            Action::Color(color) => {
                self.pen_color = color;
            },
            Action::Width(width) => {
                self.pen_width = width;
            },
            Action::Clear => {
                self.turtle_x = 0.0;
                self.turtle_y = 0.0;
                self.angle = 0.0;
                self.draw_commands.clear();
                self.output_buffer.push_str("🐢 CLEAR SCREEN\n");
            },
            Action::Home => {
                self.turtle_x = 0.0;
                self.turtle_y = 0.0;
                self.angle = 0.0;
                self.output_buffer.push_str("🐢 HOME\n");
            },
            Action::Print(msg) => {
                self.output_buffer.push_str(&format!("{}\n", msg));
            },
            Action::Repeat(count, tokens) => {
                if count > 0 {
                    self.call_stack.push(LogoFrame {
                        tokens,
                        pc: 0,
                        repeat_count: Some(count),
                        is_procedure: false,
                    });
                }
            },
            Action::If(tokens) => {
                self.call_stack.push(LogoFrame {
                    tokens,
                    pc: 0,
                    repeat_count: None,
                    is_procedure: false,
                });
            },
            Action::Call(name, args) => {
                if let Some(proc) = self.procedures.get(&name).cloned() {
                    let mut new_scope = HashMap::new();
                    for (param, arg) in proc.params.iter().zip(args.iter()) {
                        new_scope.insert(param.clone(), *arg);
                    }
                    self.scopes.push(new_scope);
                    self.call_stack.push(LogoFrame {
                        tokens: proc.body,
                        pc: 0,
                        repeat_count: None,
                        is_procedure: true,
                    });
                }
            },
            Action::Define(name, proc) => {
                self.procedures.insert(name, proc);
            },
            Action::Stop => {
                // Pop until procedure
                while let Some(frame) = self.call_stack.last() {
                    if frame.is_procedure {
                        break;
                    }
                    self.call_stack.pop();
                }
                // Pop the procedure frame too
                if let Some(frame) = self.call_stack.pop() {
                    if frame.is_procedure {
                        self.scopes.pop();
                    }
                }
                if self.call_stack.is_empty() {
                    self.state = ExecutionState::Finished;
                }
            },
            Action::Error(msg) => {
                self.output_buffer.push_str(&format!("❌ {}\n", msg));
            },
            Action::None => {}
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

impl Interpreter for LogoInterpreter {
    fn execute(&mut self, command: &str) -> String {
        let (output, _) = self.start_execution(command);
        let mut final_output = output;
        while self.state == ExecutionState::Running {
             let (out, _) = self.continue_execution();
             final_output.push_str(&out);
        }
        final_output
    }

    fn run(&mut self, code: &str) -> String {
        self.draw_commands.clear();
        self.draw_commands.push(DrawCommand::Clear { color: (0, 0, 0) });
        self.turtle_x = 0.0;
        self.turtle_y = 0.0;
        self.angle = 0.0;
        self.pen_down = true;
        self.pen_color = (255, 255, 255);
        self.pen_width = 1.0;
        self.procedures.clear();
        self.scopes = vec![HashMap::new()];
        self.execute(code)
    }

    fn start_execution(&mut self, code: &str) -> (String, ExecutionState) {
        let tokens = self.tokenize(code);
        self.call_stack.clear();
        self.call_stack.push(LogoFrame {
            tokens,
            pc: 0,
            repeat_count: None,
            is_procedure: false,
        });
        self.state = ExecutionState::Running;
        self.output_buffer.clear();
        
        self.execute_steps(100);
        (self.get_output_internal(), self.state.clone())
    }

    fn continue_execution(&mut self) -> (String, ExecutionState) {
        if self.state == ExecutionState::Finished {
            return (String::new(), ExecutionState::Finished);
        }
        self.state = ExecutionState::Running;
        self.execute_steps(100);
        (self.get_output_internal(), self.state.clone())
    }

    fn step_execution(&mut self) -> (String, ExecutionState) {
        if self.state == ExecutionState::Finished {
            return (String::new(), ExecutionState::Finished);
        }
        self.state = ExecutionState::Running;
        self.execute_steps(1);
        (self.get_output_internal(), self.state.clone())
    }

    fn get_variables(&self) -> HashMap<String, String> {
        let mut vars = HashMap::new();
        vars.insert("TURTLE_X".to_string(), format!("{:.2}", self.turtle_x));
        vars.insert("TURTLE_Y".to_string(), format!("{:.2}", self.turtle_y));
        vars.insert("ANGLE".to_string(), format!("{:.2}", self.angle));
        vars.insert("PEN_DOWN".to_string(), self.pen_down.to_string());
        vars
    }
    
    fn get_draw_commands(&self) -> Vec<DrawCommand> {
        let mut cmds = self.draw_commands.clone();
        cmds.push(DrawCommand::Turtle {
            x: self.turtle_x,
            y: self.turtle_y,
            angle: self.angle,
            visible: true,
            color: (0, 255, 0),
        });
        cmds
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_logo_forward() {
        let mut interpreter = LogoInterpreter::new();
        let code = "FD 100";
        let _ = interpreter.run(code);
        let (_x, y, _) = interpreter.get_turtle_state();
        // Initial angle 0 (up). FD 100 -> y should be 100 (positive Y is UP in our coordinate system)
        // Implementation: dy = cos(angle) * dist. cos(0) = 1. dy = 100.
        assert!((y - 100.0).abs() < 0.001);
    }

    #[test]
    fn test_logo_repeat() {
        let mut interpreter = LogoInterpreter::new();
        let code = "REPEAT 4 [ FD 100 RT 90 ]";
        let _ = interpreter.run(code);
        let (x, y, angle) = interpreter.get_turtle_state();
        // Should be back at 0,0 with angle 0 (360 % 360)
        assert!(x.abs() < 0.1);
        assert!(y.abs() < 0.1);
        assert!(angle.abs() < 0.1 || (angle - 360.0).abs() < 0.1);
    }

    #[test]
    fn test_logo_nested_repeat() {
        let mut interpreter = LogoInterpreter::new();
        let code = "REPEAT 2 [ REPEAT 2 [ FD 10 RT 90 ] ]";
        // 2 * (2 * (FD 10 RT 90)) = 4 moves.
        // FD 10, RT 90 (x=0, y=-10, a=90)
        // FD 10, RT 90 (x=10, y=-10, a=180)
        // Repeat outer 2nd time:
        // FD 10, RT 90 (x=10, y=0, a=270)
        // FD 10, RT 90 (x=0, y=0, a=0)
        let _ = interpreter.run(code);
        let (x, y, _angle) = interpreter.get_turtle_state();
        assert!(x.abs() < 0.1);
        assert!(y.abs() < 0.1);
    }

    #[test]
    fn test_logo_state_persistence() {
        let mut interpreter = LogoInterpreter::new();
        
        // Execute first command
        interpreter.execute("FD 100");
        let vars = interpreter.get_variables();
        let y = vars.get("TURTLE_Y").unwrap().parse::<f32>().unwrap();
        assert!((y - 100.0).abs() < 0.01); // y should be 100 (up)
        
        // Execute second command
        interpreter.execute("RT 90");
        let vars = interpreter.get_variables();
        let angle = vars.get("ANGLE").unwrap().parse::<f32>().unwrap();
        assert!((angle - 90.0).abs() < 0.01);
        
        // Execute third command
        interpreter.execute("FD 100");
        let vars = interpreter.get_variables();
        let x = vars.get("TURTLE_X").unwrap().parse::<f32>().unwrap();
        assert!((x - 100.0).abs() < 0.01);
    }

    #[test]
    fn test_logo_run_resets_state() {
        let mut interpreter = LogoInterpreter::new();
        
        // Execute some commands
        interpreter.execute("FD 100");
        
        // Run full script
        interpreter.run("RT 90");
        
        let vars = interpreter.get_variables();
        let y = vars.get("TURTLE_Y").unwrap().parse::<f32>().unwrap();
        let angle = vars.get("ANGLE").unwrap().parse::<f32>().unwrap();
        
        // Should be reset then executed RT 90
        assert!(y.abs() < 0.01); // y should be 0
        assert!((angle - 90.0).abs() < 0.01);
    }

    #[test]
    fn test_logo_recursive_tree() {
        let mut interpreter = LogoInterpreter::new();
        let code = "
TO TREE :SIZE
  IF :SIZE < 5 [ STOP ]
  FD :SIZE
  LT 30
  TREE :SIZE * 0.7
  RT 60
  TREE :SIZE * 0.7
  LT 30
  BK :SIZE
END

TREE 50
";
        let _ = interpreter.run(code);
        let cmds = interpreter.get_draw_commands();
        // 1 Clear + 1 Turtle + many Lines
        assert!(cmds.len() > 10); 
    }

    #[test]
    fn test_logo_color() {
        let mut interpreter = LogoInterpreter::new();
        
        // Test RGB List
        interpreter.run("SETPC [255 0 0] FD 10");
        let cmds = interpreter.get_draw_commands();
        // 0: Clear, 1: Line, 2: Turtle
        if let DrawCommand::Line { color, .. } = cmds[1] {
            assert_eq!(color, (255, 0, 0));
        } else {
            panic!("Expected Line command");
        }

        // Test Color Index (1 = Blue)
        interpreter.run("SETPC 1 FD 10");
        let cmds = interpreter.get_draw_commands();
        if let DrawCommand::Line { color, .. } = cmds[1] {
            assert_eq!(color, (0, 0, 255));
        } else {
            panic!("Expected Line command");
        }

        // Test Color Name (RED)
        interpreter.run("SETPC \"RED FD 10");
        let cmds = interpreter.get_draw_commands();
        if let DrawCommand::Line { color, .. } = cmds[1] {
            assert_eq!(color, (255, 0, 0));
        } else {
            panic!("Expected Line command");
        }
    }

    #[test]
    fn test_logo_pen_width() {
        let mut interpreter = LogoInterpreter::new();
        let code = "SETPENWIDTH 5 FD 50";
        let _ = interpreter.run(code);
        let cmds = interpreter.get_draw_commands();
        // 0: Clear, 1: Line, 2: Turtle
        assert!(cmds.len() >= 2);
        if let DrawCommand::Line { width, .. } = cmds[1] {
            assert_eq!(width, 5.0);
        } else {
            panic!("Expected Line command");
        }
    }
}
