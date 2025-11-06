use crate::graphics::TurtleState;
use crate::interpreter::ScreenMode;
use crate::interpreter::{ExecutionResult, Interpreter};
use anyhow::Result;

pub fn execute(
    interp: &mut Interpreter,
    command: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    let trimmed = command.trim();
    if trimmed.is_empty() {
        return Ok(ExecutionResult::Continue);
    }
    // Determine keyword in a case-insensitive way but preserve original args
    let mut it = trimmed.splitn(2, char::is_whitespace);
    let keyword = it.next().unwrap_or("");
    let args = it.next().unwrap_or("");
    let kw = keyword.to_uppercase();

    match kw.as_str() {
        "PRINT" => execute_print(interp, args),
        "LET" => execute_let(interp, args),
        "INPUT" => execute_input(interp, args),
        "GOTO" => execute_goto(interp, args),
        "IF" => execute_if(interp, args, turtle),
        "FOR" => execute_for(interp, args),
        "NEXT" => execute_next(interp, args),
        "GOSUB" => execute_gosub(interp, args),
        "RETURN" => execute_return(interp),
        "REM" => Ok(ExecutionResult::Continue), // Comment
        "END" => Ok(ExecutionResult::End),
        "LINE" => execute_line(interp, args, turtle),
        "CIRCLE" => execute_circle(interp, args, turtle),
        "SCREEN" => execute_screen(interp, args, turtle),
        "CLS" => execute_cls(interp),
        "LOCATE" => execute_locate(interp, args),
        _ => {
            // Allow PILOT to issue SCREEN lines by passing through to BASIC executor when keyword matches
            if keyword.eq_ignore_ascii_case("SCREEN") {
                return execute_screen(interp, args, turtle);
            }
            interp.log_output(format!("Unknown BASIC command: {}", keyword));
            Ok(ExecutionResult::Continue)
        }
    }
}

fn execute_screen(
    interp: &mut Interpreter,
    args: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    // SCREEN mode[, w, h]
    let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
    if parts.is_empty() || parts[0].is_empty() {
        interp.log_output("❌ SCREEN: Missing mode".to_string());
        return Ok(ExecutionResult::Continue);
    }
    let mode_val = interp.evaluate_expression(parts[0]).unwrap_or(0.0) as i32;
    let mut made_change = false;
    match mode_val {
        0 => {
            // Text mode
            let cols = if parts.len() > 1 {
                interp.evaluate_expression(parts[1]).unwrap_or(80.0) as u32
            } else {
                80
            };
            let rows = if parts.len() > 2 {
                interp.evaluate_expression(parts[2]).unwrap_or(25.0) as u32
            } else {
                25
            };
            interp.screen_mode = ScreenMode::Text { cols, rows };
            // Map text grid to pixel canvas for consistency
            let char_w = 10.0f32; // approximate monospace width
            let char_h = 18.0f32; // approximate line height
            turtle.canvas_width = (cols as f32 * char_w).max(200.0);
            turtle.canvas_height = (rows as f32 * char_h).max(150.0);
            made_change = true;
        }
        1 => {
            // Graphics default 640x480 unless overridden
            let mut w = 640u32;
            let mut h = 480u32;
            if parts.len() > 1 {
                w = interp.evaluate_expression(parts[1]).unwrap_or(w as f64) as u32;
            }
            if parts.len() > 2 {
                h = interp.evaluate_expression(parts[2]).unwrap_or(h as f64) as u32;
            }
            interp.screen_mode = ScreenMode::Graphics {
                width: w,
                height: h,
            };
            turtle.canvas_width = w as f32;
            turtle.canvas_height = h as f32;
            made_change = true;
        }
        2 => {
            // Graphics default 1024x768 unless overridden
            let mut w = 1024u32;
            let mut h = 768u32;
            if parts.len() > 1 {
                w = interp.evaluate_expression(parts[1]).unwrap_or(w as f64) as u32;
            }
            if parts.len() > 2 {
                h = interp.evaluate_expression(parts[2]).unwrap_or(h as f64) as u32;
            }
            interp.screen_mode = ScreenMode::Graphics {
                width: w,
                height: h,
            };
            turtle.canvas_width = w as f32;
            turtle.canvas_height = h as f32;
            made_change = true;
        }
        _ => {
            interp.log_output(format!("❌ SCREEN: Unsupported mode {}", mode_val));
        }
    }

    if made_change {
        interp.log_output(format!("🎨 SCREEN set: {:?}", interp.screen_mode));
    }
    Ok(ExecutionResult::Continue)
}

fn execute_print(interp: &mut Interpreter, args: &str) -> Result<ExecutionResult> {
    // Split by commas, respecting quotes
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_quotes = false;
    for ch in args.chars() {
        match ch {
            '"' => {
                in_quotes = !in_quotes;
                current.push(ch);
            }
            ',' if !in_quotes => {
                if !current.trim().is_empty() {
                    parts.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    if !current.trim().is_empty() {
        parts.push(current.trim().to_string());
    }

    if parts.is_empty() {
        interp.log_output(String::new());
        return Ok(ExecutionResult::Continue);
    }

    // Pre-allocate with estimated capacity
    let mut out_items: Vec<String> = Vec::with_capacity(parts.len());
    for item in parts {
        let item_trim = item.trim();
        if item_trim.starts_with('"') && item_trim.ends_with('"') && item_trim.len() >= 2 {
            // String literal - avoid allocation by using slice
            out_items.push(item_trim[1..item_trim.len() - 1].to_string());
        } else if item_trim.to_uppercase() == "INKEY$" {
            // Special handling for INKEY$
            out_items.push(interp.get_inkey());
        } else {
            // Try numeric expression first
            match interp.evaluate_expression(item_trim) {
                Ok(v) => out_items.push(v.to_string()),
                Err(_) => {
                    // Try variable lookup (string or numeric) before interpolation
                    if let Some(s) = interp.string_variables.get(item_trim) {
                        out_items.push(s.clone());
                    } else if let Some(n) = interp.variables.get(item_trim) {
                        out_items.push(n.to_string());
                    } else {
                        // Fallback: interpolate *VAR* style
                        out_items.push(interp.interpolate_text(item_trim));
                    }
                }
            }
        }
    }
    interp.log_output(out_items.join(" "));
    Ok(ExecutionResult::Continue)
}

fn execute_let(interp: &mut Interpreter, assignment: &str) -> Result<ExecutionResult> {
    if let Some(pos) = assignment.find('=') {
        let var_name = assignment[..pos].trim().to_string();
        let expr = assignment[pos + 1..].trim();

        // Special handling for INKEY$
        if expr.trim().to_uppercase() == "INKEY$" {
            let key = interp.get_inkey();
            interp.string_variables.insert(var_name, key);
            return Ok(ExecutionResult::Continue);
        }

        match interp.evaluate_expression(expr) {
            Ok(value) => {
                interp.variables.insert(var_name, value);
            }
            Err(_) => {
                // Treat as string literal or raw text
                let val = if expr.starts_with('"') && expr.ends_with('"') && expr.len() >= 2 {
                    expr[1..expr.len() - 1].to_string()
                } else {
                    expr.to_string()
                };
                interp.string_variables.insert(var_name, val);
            }
        }
    }

    Ok(ExecutionResult::Continue)
}

fn execute_input(interp: &mut Interpreter, var: &str) -> Result<ExecutionResult> {
    let var_name = var.trim().to_string();
    let prompt = format!("{}? ", var_name);

    // If an input callback is wired (tests or headless), use it synchronously
    if interp.input_callback.is_some() {
        let input_value = interp.request_input(&prompt);
        match input_value.trim().parse::<f64>() {
            Ok(num) => {
                interp.variables.insert(var_name.clone(), num);
            }
            Err(_) => {
                interp
                    .string_variables
                    .insert(var_name.clone(), input_value);
            }
        }
        return Ok(ExecutionResult::Continue);
    }

    // Otherwise, initiate a pending UI input and pause execution
    interp.start_input_request(&prompt, &var_name, true);
    Ok(ExecutionResult::WaitForInput)
}

fn execute_goto(interp: &mut Interpreter, line_num: &str) -> Result<ExecutionResult> {
    if let Ok(num) = line_num.trim().parse::<usize>() {
        if let Some(idx) = find_line_index(interp, num) {
            return Ok(ExecutionResult::Jump(idx));
        } else {
            interp.log_output(format!("❌ GOTO {} failed: line not found", num));
        }
    }
    Ok(ExecutionResult::Continue)
}

fn execute_if(
    interp: &mut Interpreter,
    condition: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    // IF <expr> THEN <command or line>
    let cond_upper = condition.to_uppercase();
    if let Some(pos) = cond_upper.find("THEN") {
        let cond_str = condition[..pos].trim();
        let then_str = condition[pos + 4..].trim();
        let truthy = interp.evaluate_expression(cond_str).unwrap_or(0.0) != 0.0;
        if truthy {
            if then_str
                .chars()
                .next()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
            {
                // THEN <line>
                return execute_goto(interp, then_str);
            } else {
                // THEN <command>
                return execute(interp, then_str, turtle);
            }
        }
    } else {
        interp.log_output("IF missing THEN".to_string());
    }
    Ok(ExecutionResult::Continue)
}

fn execute_for(interp: &mut Interpreter, params: &str) -> Result<ExecutionResult> {
    // FOR var = start TO end [STEP step]
    let params_upper = params.to_uppercase();

    // Find '=' and 'TO'
    let eq_pos = params
        .find('=')
        .ok_or_else(|| anyhow::anyhow!("FOR missing '='"))?;
    let to_pos = params_upper
        .find(" TO ")
        .ok_or_else(|| anyhow::anyhow!("FOR missing TO"))?;

    let var_name = params[..eq_pos].trim().to_string();
    let start_expr = params[eq_pos + 1..to_pos].trim();

    // Check for STEP
    let (end_expr, step_val) = if let Some(step_pos) = params_upper.find(" STEP ") {
        let end = params[to_pos + 4..step_pos].trim();
        let step = params[step_pos + 6..].trim();
        (end, interp.evaluate_expression(step)?)
    } else {
        (params[to_pos + 4..].trim(), 1.0)
    };

    let start = interp.evaluate_expression(start_expr)?;
    let end = interp.evaluate_expression(end_expr)?;

    // Initialize loop variable
    interp.variables.insert(var_name.clone(), start);

    // Push FOR context onto stack
    interp.for_stack.push(crate::interpreter::ForContext {
        var_name,
        end_value: end,
        step: step_val,
        for_line: interp.current_line,
    });

    Ok(ExecutionResult::Continue)
}

fn execute_next(interp: &mut Interpreter, var: &str) -> Result<ExecutionResult> {
    // NEXT var
    let var_name = var.trim();

    if let Some(ctx) = interp.for_stack.last() {
        // Verify variable name matches
        if !var_name.is_empty() && ctx.var_name != var_name {
            return Err(anyhow::anyhow!(
                "NEXT {} does not match FOR {}",
                var_name,
                ctx.var_name
            ));
        }

        // Get current value
        let current = interp.variables.get(&ctx.var_name).copied().unwrap_or(0.0);
        let new_val = current + ctx.step;

        // Check if loop should continue
        let should_continue = if ctx.step >= 0.0 {
            new_val <= ctx.end_value
        } else {
            new_val >= ctx.end_value
        };

        if should_continue {
            interp.variables.insert(ctx.var_name.clone(), new_val);
            let for_line = ctx.for_line;
            return Ok(ExecutionResult::Jump(for_line + 1));
        } else {
            // Loop complete, pop context
            interp.for_stack.pop();
        }
    } else {
        return Err(anyhow::anyhow!("NEXT without FOR"));
    }

    Ok(ExecutionResult::Continue)
}

fn execute_gosub(interp: &mut Interpreter, line_num: &str) -> Result<ExecutionResult> {
    if let Ok(num) = line_num.trim().parse::<usize>() {
        interp.push_gosub(interp.current_line);
        if let Some(idx) = find_line_index(interp, num) {
            return Ok(ExecutionResult::Jump(idx));
        } else {
            interp.log_output(format!("GOSUB {} failed: line not found", num));
        }
    }
    Ok(ExecutionResult::Continue)
}

fn execute_return(interp: &mut Interpreter) -> Result<ExecutionResult> {
    if let Some(line) = interp.pop_gosub() {
        Ok(ExecutionResult::Jump(line + 1))
    } else {
        interp.log_output("RETURN without GOSUB".to_string());
        Ok(ExecutionResult::Continue)
    }
}

// Helper: Find the index of a program line by BASIC line number
fn find_line_index(interp: &Interpreter, num: usize) -> Option<usize> {
    // Use line_number_map for O(1) lookup instead of O(n) scan
    interp.line_number_map.get(&num).copied()
}

fn execute_line(
    interp: &mut Interpreter,
    args: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    // LINE x1, y1, x2, y2
    let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
    if parts.len() >= 4 {
        let x1 = interp.evaluate_expression(parts[0])? as f32;
        let y1 = interp.evaluate_expression(parts[1])? as f32;
        let x2 = interp.evaluate_expression(parts[2])? as f32;
        let y2 = interp.evaluate_expression(parts[3])? as f32;

        // Draw line by moving turtle with pen down
        let old_pen = turtle.pen_down;
        turtle.pen_down = false;
        turtle.goto(x1, y1);
        turtle.pen_down = true;
        turtle.goto(x2, y2);
        turtle.pen_down = old_pen;
    }
    Ok(ExecutionResult::Continue)
}

fn execute_circle(
    interp: &mut Interpreter,
    args: &str,
    turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    // CIRCLE x, y, radius
    let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
    if parts.len() >= 3 {
        let cx = interp.evaluate_expression(parts[0])? as f32;
        let cy = interp.evaluate_expression(parts[1])? as f32;
        let r = interp.evaluate_expression(parts[2])? as f32;

        // Approximate circle with line segments
        let old_pen = turtle.pen_down;
        turtle.pen_down = false;

        let segments = 36;
        let angle_step = 360.0 / segments as f32;

        // Start at top of circle
        let start_x = cx;
        let start_y = cy + r;
        turtle.goto(start_x, start_y);
        turtle.pen_down = true;

        for i in 1..=segments {
            let angle = (i as f32 * angle_step).to_radians();
            let x = cx + r * angle.sin();
            let y = cy + r * angle.cos();
            turtle.goto(x, y);
        }

        turtle.pen_down = old_pen;
    }
    Ok(ExecutionResult::Continue)
}

fn execute_cls(interp: &mut Interpreter) -> Result<ExecutionResult> {
    // Clear screen: reset text buffer and cursor position
    interp.text_lines.clear();
    interp.cursor_row = 0;
    interp.cursor_col = 0;
    // Also log empty line to output for consistency
    interp.output.push("🎨 Screen cleared".to_string());
    Ok(ExecutionResult::Continue)
}

fn execute_locate(interp: &mut Interpreter, args: &str) -> Result<ExecutionResult> {
    // LOCATE row, col - set cursor position (1-based)
    let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
    if parts.len() >= 2 {
        let row = interp.evaluate_expression(parts[0]).unwrap_or(1.0) as u32;
        let col = interp.evaluate_expression(parts[1]).unwrap_or(1.0) as u32;
        // Convert to 0-based
        interp.cursor_row = row.saturating_sub(1);
        interp.cursor_col = col.saturating_sub(1);
    }
    Ok(ExecutionResult::Continue)
}
