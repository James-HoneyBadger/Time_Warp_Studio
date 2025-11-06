use crate::app::TimeWarpApp;
use eframe::egui;

pub fn render(app: &mut TimeWarpApp, ui: &mut egui::Ui) {
    // Unified output screen (text + graphics)
    ui.vertical(|ui| {
        ui.heading("Unified Screen");
        ui.horizontal(|ui| {
            ui.checkbox(&mut app.show_overlay_text, "Overlay text in graphics");
        });
        ui.separator();
        crate::ui::screen::render(app, ui);
    });

    // If interpreter is waiting for input, show a prompt overlay
    if let Some(req) = app.interpreter.pending_input.clone() {
        egui::Window::new("Input Required")
            .collapsible(false)
            .resizable(false)
            .anchor(egui::Align2::CENTER_CENTER, egui::vec2(0.0, 0.0))
            .show(ui.ctx(), |ui| {
                ui.label(format!("📝 {}", req.prompt));
                let response = ui.add(
                    egui::TextEdit::singleline(&mut app.input_buffer)
                        .hint_text("Type here and press Enter")
                        .desired_width(300.0),
                );

                // Check for Enter key press directly
                let enter_pressed = ui.input(|i| i.key_pressed(egui::Key::Enter));
                let should_submit = enter_pressed && response.has_focus();

                ui.horizontal(|ui| {
                    if ui.button("Submit").clicked() || should_submit {
                        let value = app.input_buffer.clone();
                        app.input_buffer.clear();
                        app.interpreter.provide_input(&value);
                        // Resume execution if we were running
                        if app.is_executing {
                            if let Err(e) = app.interpreter.execute(&mut app.turtle_state) {
                                app.error_message = Some(format!("Execution error: {}", e));
                                app.is_executing = false;
                            } else {
                                // If still waiting, remain executing; else stop
                                if app.interpreter.pending_input.is_none() {
                                    app.is_executing = false;
                                }
                            }
                        }
                    }
                    if ui.button("Cancel").clicked() {
                        // Treat cancel as empty input
                        app.input_buffer.clear();
                        app.interpreter.provide_input("");
                        if app.is_executing {
                            if let Err(e) = app.interpreter.execute(&mut app.turtle_state) {
                                app.error_message = Some(format!("Execution error: {}", e));
                                app.is_executing = false;
                            } else if app.interpreter.pending_input.is_none() {
                                app.is_executing = false;
                            }
                        }
                    }
                });
                // Request focus on first frame
                response.request_focus();
            });
    }
}
