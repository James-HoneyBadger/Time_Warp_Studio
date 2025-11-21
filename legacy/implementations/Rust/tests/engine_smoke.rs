use std::sync::{Arc, Mutex};
// Basic smoke tests for the Time Warp engine
use time_warp_rust::engine::{BufferConsole, Interpreter, SharedTurtle, TurtleModel};

fn run(code: &str) -> (String, usize) {
    let console = Arc::new(Mutex::new(String::new()));
    let turtle = Arc::new(Mutex::new(TurtleModel::new(200.0, 150.0)));
    let mut interp = Interpreter::new(
        BufferConsole::new(Arc::clone(&console)),
        SharedTurtle::new(Arc::clone(&turtle)),
    );
    if let Err(e) = interp.run(code) {
        panic!("Interpreter error: {}", e);
    }
    let out = console.lock().map(|c| c.clone()).unwrap_or_default();
    let lines = turtle.lock().map(|m| m.lines.len()).unwrap_or(0);
    (out, lines)
}

#[test]
fn prints_and_math_work() {
    let (out, lines) = run("PRINT \"Hi\"\nPRINT 1+2*3\n");
    assert!(out.contains("Hi"));
    assert!(out.contains("7"));
    assert_eq!(lines, 0);
}

#[test]
fn turtle_draws_lines() {
    let code = "CLS\nPD\nSETXY 10 10\nFD 50\nLT 90\nFD 40\n";
    let (_out, lines) = run(code);
    // Two forward moves with pen down should draw 2 line segments
    assert!(lines >= 2);
}
