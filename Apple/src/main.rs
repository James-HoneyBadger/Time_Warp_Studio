// Time Warp IDE for Apple macOS (native)
// This is a direct adaptation of the Windows/Rust IDE, with macOS-specific title and packaging.

use eframe::{egui, egui::Vec2};
use rfd::FileDialog;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Instant;

use time_warp_rust::engine::{BufferConsole, Interpreter, SharedTurtle, TurtleModel};

struct AppState {
    code: String,
    console: Arc<Mutex<String>>,
    turtle: Arc<Mutex<TurtleModel>>,
    status: String,
    running: bool,
    examples: Vec<PathBuf>,
    selected_example: usize,
    project_root: PathBuf,
    runner_handle: Option<thread::JoinHandle<()>>,
    toast: Option<(String, Instant)>,
    last_save_dir: Option<PathBuf>,
    show_about: bool,
}

impl AppState {
    fn new(project_root: PathBuf) -> Self {
        let examples_dir = project_root.join("examples");
        let mut examples = Vec::new();
        if let Ok(rd) = fs::read_dir(&examples_dir) {
            for e in rd.flatten() {
                let p = e.path();
                if p.extension().and_then(|s| s.to_str()) == Some("tc") {
                    examples.push(p);
                }
            }
        }
        examples.sort();
        let turtle_model = Arc::new(Mutex::new(TurtleModel::new(500.0, 400.0)));
        Self {
            code: String::from("REM Time Warp (Apple macOS)\nPRINT \"Hello from macOS IDE\"\n"),
            console: Arc::new(Mutex::new(String::new())),
            turtle: turtle_model,
            status: String::from("Ready"),
            running: false,
            examples,
            selected_example: 0,
            project_root: project_root.clone(),
            runner_handle: None,
            toast: None,
            last_save_dir: {
                let p = project_root.join(".time_warp_last_save_dir");
                match fs::read_to_string(&p) {
                    Ok(s) => {
                        let dir = PathBuf::from(s.trim());
                        if dir.is_dir() {
                            Some(dir)
                        } else {
                            None
                        }
                    }
                    Err(_) => None,
                }
            },
            show_about: false,
        }
    }
    // ...existing code...
}

fn main() {
    let project_root = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let native_options = eframe::NativeOptions {
        initial_window_size: Some(Vec2::new(900.0, 700.0)),
        drag_and_drop_support: true,
        follow_system_theme: true,
        ..Default::default()
    };
    eframe::run_native(
        "Time Warp (Apple macOS)",
        native_options,
        Box::new(|_cc| Box::new(AppState::new(project_root))),
    )
    .unwrap();
}
