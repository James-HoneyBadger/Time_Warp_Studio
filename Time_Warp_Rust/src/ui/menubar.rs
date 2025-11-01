use crate::app::TimeWarpApp;
use crate::ui::themes::Theme;
use eframe::egui;

pub fn render(app: &mut TimeWarpApp, ctx: &egui::Context) {
    egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
        egui::menu::bar(ui, |ui| {
            // File menu
            ui.menu_button("File", |ui| {
                if ui.button("📄 New").clicked() {
                    new_file(app);
                    ui.close_menu();
                }
                if ui.button("📂 Open...").clicked() {
                    open_file(app);
                    ui.close_menu();
                }
                if ui.button("💾 Save").clicked() {
                    save_file(app);
                    ui.close_menu();
                }
                if ui.button("💾 Save As...").clicked() {
                    save_file_as(app);
                    ui.close_menu();
                }
                ui.separator();
                if ui.button("❌ Exit").clicked() {
                    std::process::exit(0);
                }
            });

            // Edit menu
            ui.menu_button("Edit", |ui| {
                if ui.button("↶ Undo").clicked() {
                    undo(app);
                    ui.close_menu();
                }
                if ui.button("↷ Redo").clicked() {
                    redo(app);
                    ui.close_menu();
                }
                ui.separator();
                if ui.button("🔍 Find/Replace").clicked() {
                    app.show_find_replace = !app.show_find_replace;
                    ui.close_menu();
                }
            });

            // Run menu
            ui.menu_button("Run", |ui| {
                if ui.button("▶️  Run Program").clicked() {
                    run_program(app);
                    ui.close_menu();
                }
                if ui.button("⏸️ Step").clicked() {
                    step_program(app);
                    ui.close_menu();
                }
                if ui.button("⏹️ Stop").clicked() {
                    stop_program(app);
                    ui.close_menu();
                }
            });

            // View menu
            ui.menu_button("View", |ui| {
                ui.menu_button("🎨 Theme", |ui| {
                    for theme in Theme::all() {
                        if ui
                            .selectable_label(app.current_theme == theme, theme.name())
                            .clicked()
                        {
                            app.current_theme = theme;
                            ui.close_menu();
                        }
                    }
                });
                ui.separator();
                if ui.button("🐢 Clear Graphics").clicked() {
                    app.turtle_state.clear();
                    ui.close_menu();
                }
                if ui.button("💾 Save Canvas as PNG...").clicked() {
                    save_canvas_as_png(app);
                    ui.close_menu();
                }
            });

            // Help menu
            ui.menu_button("Help", |ui| {
                if ui.button("📖 Documentation").clicked() {
                    app.active_tab = 4; // Help tab
                    ui.close_menu();
                }
                if ui.button("ℹ️ About").clicked() {
                    show_about(app);
                    ui.close_menu();
                }
            });
        });
    });
}

fn new_file(app: &mut TimeWarpApp) {
    let filename = format!("untitled_{}.pilot", app.open_files.len());
    app.file_buffers.insert(filename.clone(), String::new());
    app.open_files.push(filename);
    app.current_file_index = app.open_files.len() - 1;
}

fn open_file(app: &mut TimeWarpApp) {
    if let Some(path) = rfd::FileDialog::new()
        .add_filter("PILOT", &["pilot", "pil"])
        .add_filter("BASIC", &["bas", "basic"])
        .add_filter("Logo", &["logo", "lgo"])
        .add_filter("All", &["*"])
        .pick_file()
    {
        if let Ok(content) = std::fs::read_to_string(&path) {
            let filename = path.file_name().unwrap().to_string_lossy().to_string();
            app.file_buffers.insert(filename.clone(), content);
            app.open_files.push(filename);
            app.current_file_index = app.open_files.len() - 1;
            app.last_file_path = Some(path.to_string_lossy().to_string());
        }
    }
}

fn save_file(app: &mut TimeWarpApp) {
    if let Some(ref path) = app.last_file_path {
        let code = app.current_code();
        let _ = std::fs::write(path, code);
        if let Some(file) = app.current_file().cloned() {
            app.file_modified.insert(file, false);
        }
    } else {
        save_file_as(app);
    }
}

fn save_file_as(app: &mut TimeWarpApp) {
    if let Some(path) = rfd::FileDialog::new()
        .add_filter("PILOT", &["pilot"])
        .add_filter("BASIC", &["bas"])
        .add_filter("Logo", &["logo"])
        .save_file()
    {
        let code = app.current_code();
        let _ = std::fs::write(&path, code);
        app.last_file_path = Some(path.to_string_lossy().to_string());
        if let Some(file) = app.current_file().cloned() {
            app.file_modified.insert(file, false);
        }
    }
}

fn undo(app: &mut TimeWarpApp) {
    app.undo();
}

fn redo(app: &mut TimeWarpApp) {
    app.redo();
}

fn run_program(app: &mut TimeWarpApp) {
    app.is_executing = true;
    let code = app.current_code();

    // Clear previous output and graphics
    app.interpreter.output.clear();
    app.turtle_state.clear();

    // Transfer any pending key press to interpreter for INKEY$
    if app.last_key_pressed.is_some() {
        app.interpreter.last_key_pressed = app.last_key_pressed.take();
    }

    if let Err(e) = app.interpreter.load_program(&code) {
        app.error_message = Some(format!("Failed to load program: {}", e));
        app.is_executing = false;
        return;
    }

    match app.interpreter.execute(&mut app.turtle_state) {
        Ok(_output) => {
            app.active_tab = 1; // Switch to output tab
        }
        Err(e) => {
            app.error_message = Some(format!("Execution error: {}", e));
        }
    }

    // If execution is waiting for input, keep executing flag set so UI can resume
    if app.interpreter.pending_input.is_none() {
        app.is_executing = false;
    } else {
        app.active_tab = 1;
    }
}

fn step_program(app: &mut TimeWarpApp) {
    // Enable step mode and execute one line
    app.step_mode = true;
    app.debug_mode = true;

    if !app.is_executing {
        // Start execution in step mode
        app.is_executing = true;
        let code = app.current_code();

        match app.interpreter.load_program(&code) {
            Ok(_) => {
                // Execute just one line
                match app.interpreter.execute(&mut app.turtle_state) {
                    Ok(_) => {
                        app.current_debug_line = Some(app.interpreter.current_line);
                        app.is_executing = false; // Pause after one step
                    }
                    Err(e) => {
                        app.error_message = Some(format!("Step error: {}", e));
                        app.is_executing = false;
                        app.step_mode = false;
                    }
                }
            }
            Err(e) => {
                app.error_message = Some(format!("Load error: {}", e));
                app.step_mode = false;
            }
        }
    } else {
        // Continue stepping through execution
        match app.interpreter.execute(&mut app.turtle_state) {
            Ok(_) => {
                app.current_debug_line = Some(app.interpreter.current_line);
                if app.interpreter.current_line >= app.interpreter.program_lines.len() {
                    app.is_executing = false;
                    app.step_mode = false;
                }
            }
            Err(e) => {
                app.error_message = Some(format!("Step error: {}", e));
                app.is_executing = false;
                app.step_mode = false;
            }
        }
    }
}

fn stop_program(app: &mut TimeWarpApp) {
    app.is_executing = false;
}

fn show_about(app: &mut TimeWarpApp) {
    app.show_about_dialog = true;
}

fn save_canvas_as_png(app: &mut TimeWarpApp) {
    if let Some(path) = rfd::FileDialog::new()
        .add_filter("PNG Image", &["png"])
        .set_file_name("turtle_canvas.png")
        .save_file()
    {
        match app.turtle_state.save_png(&path.to_string_lossy()) {
            Ok(_) => {
                app.error_message = Some(format!("Canvas saved to {}", path.display()));
            }
            Err(e) => {
                app.error_message = Some(format!("Failed to save PNG: {}", e));
            }
        }
    }
}
