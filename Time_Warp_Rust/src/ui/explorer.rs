use crate::app::TimeWarpApp;
use eframe::egui;

pub fn render(_app: &TimeWarpApp, ui: &mut egui::Ui) {
    ui.heading("File Explorer");
    ui.separator();

    ui.label("File explorer coming soon:");
    ui.label("• Project tree view");
    ui.label("• File operations");
    ui.label("• Drag and drop");
}
