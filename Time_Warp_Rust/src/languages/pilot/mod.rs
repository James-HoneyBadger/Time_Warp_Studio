use crate::graphics::TurtleState;
use crate::interpreter::{ExecutionResult, Interpreter};
use anyhow::Result;

pub fn execute(
    interp: &mut Interpreter,
    command: &str,
    _turtle: &mut TurtleState,
) -> Result<ExecutionResult> {
    let cmd = command.trim();

    // Determine command type from first two characters
    let cmd_type = if cmd.len() >= 2 {
        &cmd[0..2]
    } else {
        return Ok(ExecutionResult::Continue);
    };

    match cmd_type {
        "T:" => execute_text(interp, &cmd[2..]),
        "A:" => execute_accept(interp, &cmd[2..]),
        "U:" => execute_use(interp, &cmd[2..]),
        "C:" => execute_compute(interp, &cmd[2..]),
        "Y:" => execute_yes(interp, &cmd[2..]),
        "N:" => execute_no(interp, &cmd[2..]),
        "M:" => execute_match(interp, &cmd[2..]),
        "J:" => execute_jump(interp, &cmd[2..]),
        "L:" => Ok(ExecutionResult::Continue), // Label, no action
        "E:" => Ok(ExecutionResult::End),
        "R:" => execute_runtime(interp, &cmd[2..]),
        _ => {
            interp.log_output(format!("Unknown PILOT command: {}", cmd));
            Ok(ExecutionResult::Continue)
        }
    }
}

fn execute_text(interp: &mut Interpreter, text: &str) -> Result<ExecutionResult> {
    // Check if conditional output (following Y: or N:)
    if interp.last_match_set {
        interp.last_match_set = false;
        if !interp.match_flag {
            return Ok(ExecutionResult::Continue);
        }
    }

    let output = interp.interpolate_text(text.trim());
    interp.log_output(output);
    Ok(ExecutionResult::Continue)
}

fn execute_accept(interp: &mut Interpreter, var: &str) -> Result<ExecutionResult> {
    let var_name = var.trim();

    // If an input callback is wired, use it synchronously
    if interp.input_callback.is_some() {
        let input = interp.request_input(var_name);
        match input.trim().parse::<f64>() {
            Ok(num) => {
                interp.variables.insert(var_name.to_string(), num);
            }
            Err(_) => {
                interp.string_variables.insert(var_name.to_string(), input);
            }
        }
        return Ok(ExecutionResult::Continue);
    }

    // Otherwise, start pending input request and pause
    let prompt = format!("{} ", var_name);
    interp.start_input_request(&prompt, var_name, true);
    Ok(ExecutionResult::WaitForInput)
}

fn execute_use(interp: &mut Interpreter, assignment: &str) -> Result<ExecutionResult> {
    // U:VAR=expression
    if let Some(pos) = assignment.find('=') {
        let var_name = assignment[..pos].trim().to_string();
        let expr = assignment[pos + 1..].trim();

        match interp.evaluate_expression(expr) {
            Ok(value) => {
                interp.variables.insert(var_name, value);
            }
            Err(_) => {
                // Treat as string
                interp.string_variables.insert(var_name, expr.to_string());
            }
        }
    }

    Ok(ExecutionResult::Continue)
}

fn execute_compute(interp: &mut Interpreter, condition: &str) -> Result<ExecutionResult> {
    // C:condition - store for Y:/N:
    let result = evaluate_condition(interp, condition)?;
    interp.stored_condition = Some(result);
    Ok(ExecutionResult::Continue)
}

fn execute_yes(interp: &mut Interpreter, condition: &str) -> Result<ExecutionResult> {
    let result = if condition.trim().is_empty() {
        interp.stored_condition.unwrap_or(false)
    } else {
        evaluate_condition(interp, condition)?
    };

    interp.match_flag = result;
    interp.last_match_set = true;
    Ok(ExecutionResult::Continue)
}

fn execute_no(interp: &mut Interpreter, condition: &str) -> Result<ExecutionResult> {
    let result = if condition.trim().is_empty() {
        interp.stored_condition.unwrap_or(false)
    } else {
        evaluate_condition(interp, condition)?
    };

    interp.match_flag = !result;
    interp.last_match_set = true;
    Ok(ExecutionResult::Continue)
}

fn execute_match(interp: &mut Interpreter, pattern: &str) -> Result<ExecutionResult> {
    // M:pattern - match last input against pattern (case-insensitive substring match)
    let pattern = pattern.trim().to_uppercase();
    let last_input = interp.last_input.to_uppercase();

    interp.match_flag = last_input.contains(&pattern);
    interp.last_match_set = true;

    Ok(ExecutionResult::Continue)
}

fn execute_jump(interp: &mut Interpreter, label: &str) -> Result<ExecutionResult> {
    let label = label.trim();

    if let Some(line) = interp.jump_to_label(label) {
        Ok(ExecutionResult::Jump(line))
    } else {
        interp.log_output(format!("Label not found: {}", label));
        Ok(ExecutionResult::Continue)
    }
}

fn execute_runtime(interp: &mut Interpreter, command: &str) -> Result<ExecutionResult> {
    // R: commands - runtime/hardware simulation
    // TODO: Implement R: commands (SAVE, LOAD, RPI, ARDUINO, ROBOT, etc.)
    interp.log_output(format!(
        "Runtime command not yet implemented: R:{}",
        command
    ));
    Ok(ExecutionResult::Continue)
}

fn evaluate_condition(interp: &Interpreter, condition: &str) -> Result<bool> {
    // Simple condition evaluator
    // Supports: var=value, var>value, var<value, var>=value, var<=value, var<>value

    for op in &[">=", "<=", "<>", "=", ">", "<"] {
        if let Some(pos) = condition.find(op) {
            let left = condition[..pos].trim();
            let right = condition[pos + op.len()..].trim();

            let left_val = interp.evaluate_expression(left)?;
            let right_val = interp.evaluate_expression(right)?;

            return Ok(match *op {
                "=" => (left_val - right_val).abs() < f64::EPSILON,
                ">" => left_val > right_val,
                "<" => left_val < right_val,
                ">=" => left_val >= right_val,
                "<=" => left_val <= right_val,
                "<>" => (left_val - right_val).abs() >= f64::EPSILON,
                _ => false,
            });
        }
    }

    Ok(false)
}
