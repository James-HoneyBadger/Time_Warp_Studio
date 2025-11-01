use crate::app::TimeWarpApp;
use eframe::egui;

pub fn render_tab_bar(app: &mut TimeWarpApp, ui: &mut egui::Ui) {
    ui.horizontal(|ui| {
        if ui
            .selectable_label(app.active_tab == 0, "📝 Editor")
            .clicked()
        {
            app.active_tab = 0;
        }
        if ui
            .selectable_label(app.active_tab == 1, "📊 Output & Graphics")
            .clicked()
        {
            app.active_tab = 1;
        }
        if ui
            .selectable_label(app.active_tab == 2, "🐛 Debug")
            .clicked()
        {
            app.active_tab = 2;
        }
        if ui
            .selectable_label(app.active_tab == 3, "📁 Explorer")
            .clicked()
        {
            app.active_tab = 3;
        }
        if ui
            .selectable_label(app.active_tab == 4, "❓ Help")
            .clicked()
        {
            app.active_tab = 4;
        }
    });
}

pub fn render(app: &mut TimeWarpApp, ui: &mut egui::Ui) {
    // File tabs
    ui.horizontal(|ui| {
        let mut to_close = None;

        for (idx, file) in app.open_files.iter().enumerate() {
            let selected = idx == app.current_file_index;
            let modified = app.file_modified.get(file).copied().unwrap_or(false);
            let label = if modified {
                format!("● {}", file)
            } else {
                file.clone()
            };

            if ui.selectable_label(selected, label).clicked() {
                app.current_file_index = idx;
            }

            if ui.small_button("✖").clicked() {
                to_close = Some(idx);
            }
        }

        if let Some(idx) = to_close {
            let file = app.open_files.remove(idx);
            app.file_buffers.remove(&file);
            app.file_modified.remove(&file);
            if app.current_file_index >= app.open_files.len() && app.current_file_index > 0 {
                app.current_file_index -= 1;
            }
        }

        if ui.button("➕").clicked() {
            let filename = format!("untitled_{}.pilot", app.open_files.len());
            app.file_buffers.insert(filename.clone(), String::new());
            app.open_files.push(filename);
            app.current_file_index = app.open_files.len() - 1;
        }
    });

    ui.separator();

    // Code editor
    let mut code = app.current_code();

    egui::ScrollArea::vertical().show(ui, |ui| {
        let response = ui.add(
            egui::TextEdit::multiline(&mut code)
                .font(egui::TextStyle::Monospace)
                .desired_width(f32::INFINITY)
                .desired_rows(30)
                .code_editor(),
        );

        if response.changed() {
            app.set_current_code(code);
        }
    });
}

pub fn render_find_replace(app: &mut TimeWarpApp, ctx: &egui::Context) {
    let mut should_find = false;
    let mut should_replace = false;
    let mut should_replace_all = false;

    egui::Window::new("Find/Replace")
        .open(&mut app.show_find_replace)
        .show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("Find:");
                ui.text_edit_singleline(&mut app.find_text);
            });
            ui.horizontal(|ui| {
                ui.label("Replace:");
                ui.text_edit_singleline(&mut app.replace_text);
            });
            ui.horizontal(|ui| {
                if ui.button("Find Next").clicked() {
                    should_find = true;
                }
                if ui.button("Replace").clicked() {
                    should_replace = true;
                }
                if ui.button("Replace All").clicked() {
                    should_replace_all = true;
                }
            });
        });

    if should_find {
        find_next(app);
    }
    if should_replace {
        replace_current(app);
    }
    if should_replace_all {
        replace_all(app);
    }
}

fn find_next(app: &mut TimeWarpApp) {
    if app.find_text.is_empty() {
        return;
    }
    let code = app.current_code();
    if code.contains(&app.find_text) {
        app.error_message = Some(format!("Found '{}' in code", app.find_text));
    } else {
        app.error_message = Some(format!("'{}' not found", app.find_text));
    }
}

fn replace_current(app: &mut TimeWarpApp) {
    if app.find_text.is_empty() {
        return;
    }
    let code = app.current_code();
    if let Some(pos) = code.find(&app.find_text) {
        let new_code = format!(
            "{}{}{}",
            &code[..pos],
            &app.replace_text,
            &code[pos + app.find_text.len()..]
        );
        app.set_current_code(new_code);
        app.error_message = Some("Replaced one occurrence".to_string());
    } else {
        app.error_message = Some(format!("'{}' not found", app.find_text));
    }
}

fn replace_all(app: &mut TimeWarpApp) {
    if app.find_text.is_empty() {
        return;
    }
    let code = app.current_code();
    let count = code.matches(&app.find_text).count();
    if count > 0 {
        let new_code = code.replace(&app.find_text, &app.replace_text);
        app.set_current_code(new_code);
        app.error_message = Some(format!("Replaced {} occurrence(s)", count));
    } else {
        app.error_message = Some(format!("'{}' not found", app.find_text));
    }
}
