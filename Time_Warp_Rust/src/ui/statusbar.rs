use crate::app::TimeWarpApp;
use eframe::egui;

pub fn render(app: &TimeWarpApp, ctx: &egui::Context) {
    egui::TopBottomPanel::bottom("status_bar").show(ctx, |ui| {
        ui.horizontal(|ui| {
            ui.label(format!(
                "File: {}",
                app.current_file().unwrap_or(&"None".to_string())
            ));
            ui.separator();

            let lang_name = if let Some(file) = app.current_file() {
                let ext = std::path::Path::new(file)
                    .extension()
                    .and_then(|e| e.to_str())
                    .unwrap_or("pilot");
                crate::languages::Language::from_extension(ext)
                    .name()
                    .to_string()
            } else {
                "PILOT".to_string()
            };
            ui.label(format!("Language: {}", lang_name));
            ui.separator();

            ui.label(format!("Theme: {}", app.current_theme.name()));
            ui.separator();

            if app.is_executing {
                ui.spinner();
                ui.label("Executing...");
            } else {
                ui.label("Ready");
            }

            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(format!("Time Warp IDE v{}", env!("CARGO_PKG_VERSION")));
            });
        });
    });
}
