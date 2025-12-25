#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app;
mod core;
mod languages;
mod ui;
mod repl;

use eframe::egui;

fn main() -> eframe::Result<()> {
    env_logger::init();

    let args: Vec<String> = std::env::args().collect();
    // Check for CLI flag or missing display on Linux
    if args.contains(&"--cli".to_string()) || 
       (std::env::consts::OS == "linux" && std::env::var("DISPLAY").is_err() && std::env::var("WAYLAND_DISPLAY").is_err()) {
        repl::run();
        return Ok(());
    }

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1024.0, 768.0])
            .with_title("Time Warp Studio"),
        ..Default::default()
    };

    eframe::run_native(
        "Time Warp Studio",
        options,
        Box::new(|cc| Ok(Box::new(app::TimeWarpApp::new(cc)))),
    )
}
