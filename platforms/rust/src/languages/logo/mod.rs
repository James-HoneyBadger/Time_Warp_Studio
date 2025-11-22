use crate::graphics::TurtleState;
use crate::interpreter::{ExecutionResult, Interpreter};
use anyhow::Result;
use std::collections::HashMap;

#[derive(Clone)]
pub struct LogoProcedure {
    pub params: Vec<String>, // Uppercase names without ':'
    pub body: Vec<String>,
}

pub fn execute(
    interp: &mut Interpreter,
    command: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    let cmd = command.trim().to_uppercase();
    let parts: Vec<&str> = cmd.splitn(2, char::is_whitespace).collect();

    if parts.is_empty() {
        return Ok(ExecutionResult::Continue);
    }
    // User-defined procedure names take precedence over built-in keywords
    let proc_upper = parts[0].to_uppercase();
    if interp.logo_procedures.contains_key(&proc_upper) {
        let arg_str = parts.get(1).copied().unwrap_or("");
        return execute_procedure(interp, &proc_upper, arg_str, turtle);
    }

    match parts[0] {
        "FORWARD" | "FD" => execute_forward(interp, turtle, parts.get(1).unwrap_or(&"0")),
        "BACK" | "BK" | "BACKWARD" => execute_back(interp, turtle, parts.get(1).unwrap_or(&"0")),
        "LEFT" | "LT" => execute_left(interp, turtle, parts.get(1).unwrap_or(&"0")),
        "RIGHT" | "RT" => execute_right(interp, turtle, parts.get(1).unwrap_or(&"0")),
        "PENUP" | "PU" => execute_penup(turtle),
        "PENDOWN" | "PD" => execute_pendown(turtle),
        "CLEARSCREEN" | "CS" => execute_clearscreen(turtle),
        "HOME" => execute_home(turtle),
        "SETXY" => execute_setxy(interp, turtle, parts.get(1).unwrap_or(&"")),
        "SETHEADING" | "SETH" => execute_setheading(interp, turtle, parts.get(1).unwrap_or(&"0")),
        "SETCOLOR" | "SETPENCOLOR" => execute_setcolor(interp, turtle, parts.get(1).unwrap_or(&"")),
        "PENWIDTH" | "SETPENSIZE" => execute_penwidth(interp, turtle, parts.get(1).unwrap_or(&"")),
        "SETBGCOLOR" => execute_setbgcolor(interp, turtle, parts.get(1).unwrap_or(&"")),
        "HIDETURTLE" | "HT" => execute_hideturtle(turtle),
        "SHOWTURTLE" | "ST" => execute_showturtle(turtle),
        "REPEAT" => execute_repeat(interp, parts.get(1).unwrap_or(&""), turtle),
        "TO" => execute_to(interp, parts.get(1).unwrap_or(&"")),
        "END" => Ok(ExecutionResult::Continue), // END handled in execute_to
        _ => {
            // Unknown command (user procedures already handled before match)
            interp.log_output(format!("❌ Unknown Logo command: {}", parts[0]));
            Ok(ExecutionResult::Continue)
        }
    }
}

fn execute_forward(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    distance_str: &str,
) -> Result<ExecutionResult> {
    let distance = eval_logo_expr(interp, distance_str.trim())?;
    turtle.forward(distance as f32);
    Ok(ExecutionResult::Continue)
}

fn execute_back(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    distance_str: &str,
) -> Result<ExecutionResult> {
    let distance = eval_logo_expr(interp, distance_str.trim())?;
    turtle.back(distance as f32);
    Ok(ExecutionResult::Continue)
}

fn execute_left(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    angle_str: &str,
) -> Result<ExecutionResult> {
    let angle = eval_logo_expr(interp, angle_str.trim())? as f32;
    turtle.left(angle);
    Ok(ExecutionResult::Continue)
}

fn execute_right(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    angle_str: &str,
) -> Result<ExecutionResult> {
    let angle = eval_logo_expr(interp, angle_str.trim())? as f32;
    turtle.right(angle);
    Ok(ExecutionResult::Continue)
}

fn execute_penup(turtle: &mut TurtleState) -> Result<ExecutionResult> {
    turtle.pen_down = false;
    Ok(ExecutionResult::Continue)
}

fn execute_pendown(turtle: &mut TurtleState) -> Result<ExecutionResult> {
    turtle.pen_down = true;
    Ok(ExecutionResult::Continue)
}

fn execute_clearscreen(turtle: &mut TurtleState) -> Result<ExecutionResult> {
    turtle.clear();
    turtle.home();
    Ok(ExecutionResult::Continue)
}

fn execute_home(turtle: &mut TurtleState) -> Result<ExecutionResult> {
    turtle.home();
    Ok(ExecutionResult::Continue)
}

fn execute_setxy(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    coords: &str,
) -> Result<ExecutionResult> {
    let parts: Vec<&str> = coords.split_whitespace().collect();
    if parts.len() >= 2 {
        let x = eval_logo_expr(interp, parts[0])? as f32;
        let y = eval_logo_expr(interp, parts[1])? as f32;
        turtle.goto(x, y);
    }
    Ok(ExecutionResult::Continue)
}

fn execute_setheading(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    angle_str: &str,
) -> Result<ExecutionResult> {
    let angle = eval_logo_expr(interp, angle_str.trim())? as f32;
    turtle.heading = angle;
    Ok(ExecutionResult::Continue)
}

fn execute_setcolor(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    args: &str,
) -> Result<ExecutionResult> {
    // SETCOLOR accepts: r g b (0-255), named color (RED, BLUE), or hex (#RRGGBB, #RGB)
    let trimmed = args.trim();
    let parts: Vec<&str> = trimmed.split_whitespace().collect();

    if parts.len() == 1 {
        let arg = parts[0].to_uppercase();
        // Check named color
        if let Some(color) = parse_named_color(&arg) {
            turtle.pen_color = color;
            return Ok(ExecutionResult::Continue);
        }
        // Check hex color
        if trimmed.starts_with('#') {
            if let Some(color) = parse_hex_color(trimmed) {
                turtle.pen_color = color;
                return Ok(ExecutionResult::Continue);
            }
        }
    } else if parts.len() >= 3 {
        // RGB values
        let r = eval_logo_expr(interp, parts[0])?.clamp(0.0, 255.0) as u8;
        let g = eval_logo_expr(interp, parts[1])?.clamp(0.0, 255.0) as u8;
        let b = eval_logo_expr(interp, parts[2])?.clamp(0.0, 255.0) as u8;
        turtle.pen_color = egui::Color32::from_rgb(r, g, b);
    }
    Ok(ExecutionResult::Continue)
}

fn execute_penwidth(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    arg: &str,
) -> Result<ExecutionResult> {
    let w = eval_logo_expr(interp, arg.trim())?.max(0.1) as f32;
    turtle.pen_width = w;
    Ok(ExecutionResult::Continue)
}

fn execute_setbgcolor(
    interp: &mut Interpreter,
    turtle: &mut TurtleState,
    args: &str,
) -> Result<ExecutionResult> {
    let trimmed = args.trim();
    let parts: Vec<&str> = trimmed.split_whitespace().collect();

    if parts.len() == 1 {
        let arg = parts[0].to_uppercase();
        if let Some(color) = parse_named_color(&arg) {
            turtle.bg_color = color;
            return Ok(ExecutionResult::Continue);
        }
        if trimmed.starts_with('#') {
            if let Some(color) = parse_hex_color(trimmed) {
                turtle.bg_color = color;
                return Ok(ExecutionResult::Continue);
            }
        }
    } else if parts.len() >= 3 {
        let r = eval_logo_expr(interp, parts[0])?.clamp(0.0, 255.0) as u8;
        let g = eval_logo_expr(interp, parts[1])?.clamp(0.0, 255.0) as u8;
        let b = eval_logo_expr(interp, parts[2])?.clamp(0.0, 255.0) as u8;
        turtle.bg_color = egui::Color32::from_rgb(r, g, b);
    }
    Ok(ExecutionResult::Continue)
}

fn execute_hideturtle(turtle: &mut TurtleState) -> Result<ExecutionResult> {
    turtle.visible = false;
    Ok(ExecutionResult::Continue)
}

fn execute_showturtle(turtle: &mut TurtleState) -> Result<ExecutionResult> {
    turtle.visible = true;
    Ok(ExecutionResult::Continue)
}

fn execute_repeat(
    interp: &mut Interpreter,
    params: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    // REPEAT n [commands] - supports nested brackets
    let params = params.trim();

    // Find count and bracket section
    let bracket_start = params
        .find('[')
        .ok_or_else(|| anyhow::anyhow!("REPEAT missing '['"))?;

    let count_str = params[..bracket_start].trim();
    let count = eval_logo_expr(interp, count_str)? as usize;

    // Extract balanced bracket content
    let commands = extract_bracket_content(&params[bracket_start..])?;

    // Parse commands into a list (handles nested REPEAT)
    let cmd_list = parse_commands(&commands)?;

    // Execute commands count times using same turtle
    for _ in 0..count {
        for cmd in &cmd_list {
            execute(interp, cmd, turtle)?;
        }
    }

    Ok(ExecutionResult::Continue)
}

/// Extract content between balanced brackets (including nested ones)
fn extract_bracket_content(text: &str) -> Result<String> {
    let mut depth = 0;
    let mut start_idx = None;
    let mut end_idx = None;

    for (i, ch) in text.chars().enumerate() {
        match ch {
            '[' => {
                if depth == 0 {
                    start_idx = Some(i + 1);
                }
                depth += 1;
            }
            ']' => {
                depth -= 1;
                if depth == 0 {
                    end_idx = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }

    if let (Some(start), Some(end)) = (start_idx, end_idx) {
        Ok(text[start..end].trim().to_string())
    } else {
        Err(anyhow::anyhow!("Unbalanced brackets in REPEAT"))
    }
}

/// Parse commands from a block (splits on whitespace but respects brackets)
fn parse_commands(block: &str) -> Result<Vec<String>> {
    let mut commands = Vec::new();
    let mut current = String::new();
    let mut depth: i32 = 0;
    let tokens = block.split_whitespace();

    for token in tokens {
        // Decide if we should start a new command before appending this token
        let starts_upper = token
            .chars()
            .next()
            .map(|c| c.is_ascii_uppercase())
            .unwrap_or(false);
        if depth == 0 && starts_upper && !current.is_empty() {
            // New top-level command starts; flush current
            commands.push(current.trim().to_string());
            current.clear();
        }

        if !current.is_empty() {
            current.push(' ');
        }
        current.push_str(token);

        // Update bracket depth after appending token
        depth += token.chars().filter(|&c| c == '[').count() as i32;
        depth -= token.chars().filter(|&c| c == ']').count() as i32;
    }

    if !current.is_empty() {
        commands.push(current.trim().to_string());
    }

    Ok(commands)
}

fn execute_to(interp: &mut Interpreter, name_and_params: &str) -> Result<ExecutionResult> {
    // TO <name> [:param ...]: collect subsequent lines until END
    let tokens: Vec<&str> = name_and_params.split_whitespace().collect();
    if tokens.is_empty() {
        return Err(anyhow::anyhow!("TO missing procedure name"));
    }
    let proc_name = tokens[0].trim().to_uppercase();
    if proc_name.is_empty() {
        return Err(anyhow::anyhow!("TO missing procedure name"));
    }
    // Parse params
    let mut params: Vec<String> = Vec::new();
    for t in tokens.iter().skip(1) {
        let t = t.trim();
        if let Some(stripped) = t.strip_prefix(':') {
            params.push(stripped.to_uppercase());
        } else {
            params.push(t.to_uppercase());
        }
    }

    let mut body: Vec<String> = Vec::new();
    let start_line = interp.current_line + 1;

    // Collect lines until END (skip TO line itself)
    for idx in start_line..interp.program_lines.len() {
        let (_, line) = &interp.program_lines[idx];
        let upper = line.trim().to_uppercase();
        if upper == "END" {
            // Store procedure and jump past END
            interp
                .logo_procedures
                .insert(proc_name.clone(), LogoProcedure { params, body });
            interp.current_line = idx;
            return Ok(ExecutionResult::Continue);
        }
        body.push(line.clone());
    }

    Err(anyhow::anyhow!("TO {} missing END", proc_name))
}

fn execute_procedure(
    interp: &mut Interpreter,
    name: &str,
    arg_str: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    // Execute stored procedure body with optional args
    if let Some(proc_def) = interp.logo_procedures.get(name).cloned() {
        // Bind parameters
        let args: Vec<&str> = arg_str.split_whitespace().collect();
        let mut old_num: HashMap<String, Option<f64>> = HashMap::new();
        let mut old_str: HashMap<String, Option<String>> = HashMap::new();
        for (i, p) in proc_def.params.iter().enumerate() {
            // Save old values
            old_num.insert(p.clone(), interp.variables.get(p).copied());
            old_str.insert(p.clone(), interp.string_variables.get(p).cloned());
            // Bind argument
            if let Some(arg) = args.get(i) {
                let tok = arg.trim();
                if tok.len() >= 2 && tok.starts_with('"') && tok.ends_with('"') {
                    // Quoted string
                    interp
                        .string_variables
                        .insert(p.clone(), tok[1..tok.len() - 1].to_string());
                    interp.variables.remove(p);
                } else if let Ok(val) = eval_logo_expr(interp, tok) {
                    // Numeric
                    interp.variables.insert(p.clone(), val);
                    interp.string_variables.remove(p);
                } else {
                    // Fallback: raw token as string
                    interp.string_variables.insert(p.clone(), tok.to_string());
                    interp.variables.remove(p);
                }
            } else {
                // Default 0 for numeric, remove string
                interp.variables.insert(p.clone(), 0.0);
                interp.string_variables.remove(p);
            }
        }
        // Execute body
        for line in proc_def.body {
            execute(interp, &line, turtle)?;
        }
        // Restore old vars
        for (k, v) in old_num.into_iter() {
            if let Some(val) = v {
                interp.variables.insert(k.clone(), val);
            } else {
                interp.variables.remove(&k);
            }
        }
        for (k, v) in old_str.into_iter() {
            if let Some(val) = v {
                interp.string_variables.insert(k.clone(), val);
            } else {
                interp.string_variables.remove(&k);
            }
        }
        Ok(ExecutionResult::Continue)
    } else {
        Err(anyhow::anyhow!("Procedure {} not found", name))
    }
}

fn parse_named_color(name: &str) -> Option<egui::Color32> {
    match name {
        "BLACK" => Some(egui::Color32::BLACK),
        "WHITE" => Some(egui::Color32::WHITE),
        "RED" => Some(egui::Color32::from_rgb(255, 0, 0)),
        "GREEN" => Some(egui::Color32::from_rgb(0, 255, 0)),
        "BLUE" => Some(egui::Color32::from_rgb(0, 0, 255)),
        "YELLOW" => Some(egui::Color32::from_rgb(255, 255, 0)),
        "CYAN" => Some(egui::Color32::from_rgb(0, 255, 255)),
        "MAGENTA" => Some(egui::Color32::from_rgb(255, 0, 255)),
        "ORANGE" => Some(egui::Color32::from_rgb(255, 165, 0)),
        "PURPLE" => Some(egui::Color32::from_rgb(128, 0, 128)),
        "PINK" => Some(egui::Color32::from_rgb(255, 192, 203)),
        "BROWN" => Some(egui::Color32::from_rgb(165, 42, 42)),
        "GRAY" | "GREY" => Some(egui::Color32::GRAY),
        _ => None,
    }
}

fn parse_hex_color(hex: &str) -> Option<egui::Color32> {
    let hex = hex.trim_start_matches('#');

    if hex.len() == 6 {
        // #RRGGBB
        let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
        let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
        let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
        Some(egui::Color32::from_rgb(r, g, b))
    } else if hex.len() == 3 {
        // #RGB -> #RRGGBB
        let r = u8::from_str_radix(&hex[0..1].repeat(2), 16).ok()?;
        let g = u8::from_str_radix(&hex[1..2].repeat(2), 16).ok()?;
        let b = u8::from_str_radix(&hex[2..3].repeat(2), 16).ok()?;
        Some(egui::Color32::from_rgb(r, g, b))
    } else {
        None
    }
}

fn eval_logo_expr(interp: &Interpreter, expr: &str) -> anyhow::Result<f64> {
    // Replace occurrences of :VAR with VAR to align with evaluator variables
    let mut sanitized = String::with_capacity(expr.len());
    let mut chars = expr.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == ':' {
            // Collect identifier
            let mut name = String::new();
            while let Some(&c) = chars.peek() {
                if c.is_alphanumeric() || c == '_' {
                    name.push(c);
                    chars.next();
                } else {
                    break;
                }
            }
            if !name.is_empty() {
                sanitized.push_str(&name.to_uppercase());
                continue;
            } else {
                // Lone ':'
                sanitized.push(ch);
                continue;
            }
        }
        sanitized.push(ch);
    }
    interp.evaluate_expression(&sanitized)
}
