#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app;
mod core;
mod languages;
mod ui;

use eframe::egui;

fn main() -> eframe::Result<()> {
    env_logger::init();

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
