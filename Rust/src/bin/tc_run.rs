use std::{
    env, fs,
    sync::{Arc, Mutex},
};
use time_warp_rust::engine::{BufferConsole, Interpreter, SharedTurtle, TurtleModel};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: tc_run <file.tc>");
        std::process::exit(1);
    }
    let path = &args[1];
    let code = fs::read_to_string(path).expect("failed to read file");

    let console = Arc::new(Mutex::new(String::new()));
    let turtle = Arc::new(Mutex::new(TurtleModel::new(500.0, 400.0)));

    let console_adapter = BufferConsole::new(Arc::clone(&console));
    let turtle_adapter = SharedTurtle::new(Arc::clone(&turtle));

    let mut interp = Interpreter::new(console_adapter, turtle_adapter);
    if let Err(e) = interp.run(&code) {
        eprintln!("Error: {}", e);
        std::process::exit(2);
    }

    let out = console.lock().map(|g| g.clone()).unwrap_or_default();
    print!("{}", out);

    let lines_drawn = turtle.lock().map(|m| m.lines.len()).unwrap_or(0);
    eprintln!("[lines_drawn={}]", lines_drawn);
}
