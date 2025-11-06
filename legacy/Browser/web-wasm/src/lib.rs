use std::sync::{Arc, Mutex};

use serde::Serialize;
use wasm_bindgen::prelude::*;

use time_warp_rust::engine::{BufferConsole, Interpreter, SharedTurtle, TurtleModel};

#[derive(Serialize)]
pub struct Line {
    pub x1: f32,
    pub y1: f32,
    pub x2: f32,
    pub y2: f32,
    pub color: (u8, u8, u8),
}

#[derive(Serialize)]
pub struct RunResult {
    pub console: String,
    pub width: f32,
    pub height: f32,
    pub lines: Vec<Line>,
}

#[wasm_bindgen]
pub fn run_code(code: &str) -> JsValue {
    // Prepare shared console and turtle model
    let console_buf = Arc::new(Mutex::new(String::new()));
    let turtle_model = Arc::new(Mutex::new(TurtleModel::new(500.0, 400.0)));

    let console_adapter = BufferConsole::new(Arc::clone(&console_buf));
    let turtle_adapter = SharedTurtle::new(Arc::clone(&turtle_model));

    let mut interp = Interpreter::new(console_adapter, turtle_adapter);
    let _ = interp.run(code);

    let console = console_buf.lock().unwrap().clone();
    let model = turtle_model.lock().unwrap().clone();
    let lines: Vec<Line> = model
        .lines
        .iter()
        .map(|s| Line {
            x1: s.x1,
            y1: s.y1,
            x2: s.x2,
            y2: s.y2,
            color: (s.color.0, s.color.1, s.color.2),
        })
        .collect();

    let result = RunResult {
        console,
        width: model.width,
        height: model.height,
        lines,
    };

    serde_wasm_bindgen::to_value(&result).unwrap()
}
