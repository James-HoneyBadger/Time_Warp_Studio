use eframe::egui;
use crate::core::interpreter::{Interpreter, DrawCommand, ExecutionState};
use crate::languages::basic::BasicInterpreter;
use crate::languages::pilot::PilotInterpreter;
use crate::languages::logo::LogoInterpreter;
use crate::languages::pascal::PascalInterpreter;
use crate::languages::forth::ForthInterpreter;
use crate::languages::prolog::PrologInterpreter;
use crate::languages::c::CInterpreter;
use crate::ui::syntax;
use crate::ui::file_dialog::{FileDialog, DialogMode};

pub struct TimeWarpApp {
    code: String,
    output: String,
    language: Language,
    
    // Interpreters
    basic_interpreter: BasicInterpreter,
    pilot_interpreter: PilotInterpreter,
    logo_interpreter: LogoInterpreter,
    pascal_interpreter: PascalInterpreter,
    forth_interpreter: ForthInterpreter,
    prolog_interpreter: PrologInterpreter,
    c_interpreter: CInterpreter,
    
    // UI State
    status_message: String,
    draw_commands: Vec<DrawCommand>,
    active_tab: Tab,
    theme: Theme,
    file_dialog: FileDialog,
    
    // Execution State
    execution_state: ExecutionState,
    input_buffer: String,
    immediate_code: String,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Language {
    Basic,
    Pilot,
    Logo,
    Pascal,
    Prolog,
    Forth,
    C,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Tab {
    Output,
    Canvas,
    Variables,
    Debug,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Theme {
    Dark,
    Light,
    Retro,
}

impl TimeWarpApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        setup_custom_fonts(&cc.egui_ctx);
        
        // Default to Dark theme
        cc.egui_ctx.set_visuals(egui::Visuals::dark());

        Self {
            code: "PRINT \"Hello from Rust!\"\nX = 42\nPRINT X".to_owned(),
            output: String::new(),
            language: Language::Basic,
            basic_interpreter: BasicInterpreter::new(),
            pilot_interpreter: PilotInterpreter::new(),
            logo_interpreter: LogoInterpreter::new(),
            pascal_interpreter: PascalInterpreter::new(),
            forth_interpreter: ForthInterpreter::new(),
            prolog_interpreter: PrologInterpreter::new(),
            c_interpreter: CInterpreter::new(),
            status_message: "Ready".to_owned(),
            draw_commands: Vec::new(),
            active_tab: Tab::Output,
            theme: Theme::Dark,
            file_dialog: FileDialog::new(),
            execution_state: ExecutionState::Finished,
            input_buffer: String::new(),
            immediate_code: String::new(),
        }
    }

    fn update_draw_commands(&mut self) {
        let cmds = match self.language {
            Language::Basic => self.basic_interpreter.get_draw_commands(),
            Language::Pilot => self.pilot_interpreter.get_draw_commands(),
            Language::Logo => self.logo_interpreter.get_draw_commands(),
            Language::Pascal => self.pascal_interpreter.get_draw_commands(),
            Language::Forth => self.forth_interpreter.get_draw_commands(),
            Language::Prolog => self.prolog_interpreter.get_draw_commands(),
            Language::C => self.c_interpreter.get_draw_commands(),
        };
        self.draw_commands = cmds;
        
        if !self.draw_commands.is_empty() {
            self.active_tab = Tab::Canvas;
        }
    }

    fn run_immediate(&mut self) {
        if self.immediate_code.trim().is_empty() {
            return;
        }
        
        let code = self.immediate_code.clone();
        self.immediate_code.clear();
        self.output.push_str(&format!("> {}\n", code));
        
        let output = match self.language {
            Language::Basic => self.basic_interpreter.execute(&code),
            Language::Pilot => self.pilot_interpreter.execute(&code),
            Language::Logo => self.logo_interpreter.execute(&code),
            Language::Pascal => self.pascal_interpreter.execute(&code),
            Language::Forth => self.forth_interpreter.execute(&code),
            Language::Prolog => self.prolog_interpreter.execute(&code),
            Language::C => self.c_interpreter.execute(&code),
        };
        
        self.output.push_str(&output);
        self.update_draw_commands();
    }

    fn submit_input(&mut self) {
        let input = self.input_buffer.clone();
        self.input_buffer.clear();
        
        match self.language {
            Language::Basic => self.basic_interpreter.provide_input(&input),
            Language::Pilot => self.pilot_interpreter.provide_input(&input),
            Language::Logo => self.logo_interpreter.provide_input(&input),
            Language::Pascal => self.pascal_interpreter.provide_input(&input),
            Language::Forth => self.forth_interpreter.provide_input(&input),
            Language::Prolog => self.prolog_interpreter.provide_input(&input),
            Language::C => self.c_interpreter.provide_input(&input),
        }
        
        self.continue_execution();
    }

    fn run_code(&mut self) {
        self.output.clear();
        self.draw_commands.clear();
        self.status_message = "Running...".to_string();
        
        let (output, state) = match self.language {
            Language::Basic => self.basic_interpreter.start_execution(&self.code),
            Language::Pilot => self.pilot_interpreter.start_execution(&self.code),
            Language::Logo => self.logo_interpreter.start_execution(&self.code),
            Language::Pascal => self.pascal_interpreter.start_execution(&self.code),
            Language::Forth => self.forth_interpreter.start_execution(&self.code),
            Language::Prolog => self.prolog_interpreter.start_execution(&self.code),
            Language::C => self.c_interpreter.start_execution(&self.code),
        };
        
        self.output = output;
        self.execution_state = state;
        self.update_draw_commands();
        
        if let ExecutionState::Finished = self.execution_state {
            self.status_message = "Execution finished.".to_string();
        } else if let ExecutionState::WaitingForInput(_) = self.execution_state {
            self.status_message = "Waiting for input...".to_string();
            self.active_tab = Tab::Output;
        }
    }
    fn continue_execution(&mut self) {
        let (output, state) = match self.language {
            Language::Basic => self.basic_interpreter.continue_execution(),
            Language::Pilot => self.pilot_interpreter.continue_execution(),
            Language::Logo => self.logo_interpreter.continue_execution(),
            Language::Pascal => self.pascal_interpreter.continue_execution(),
            Language::Forth => self.forth_interpreter.continue_execution(),
            Language::Prolog => self.prolog_interpreter.continue_execution(),
            Language::C => self.c_interpreter.continue_execution(),
        };
        
        self.output.push_str(&output);
        self.execution_state = state;
        self.update_draw_commands();
        
        if let ExecutionState::Finished = self.execution_state {
            self.status_message = "Execution finished.".to_string();
        } else if let ExecutionState::WaitingForInput(_) = self.execution_state {
            self.status_message = "Waiting for input...".to_string();
            self.active_tab = Tab::Output;
        }
    }
    fn step_code(&mut self) {
        if let ExecutionState::Finished = self.execution_state {
            self.output.clear();
            self.draw_commands.clear();
            self.status_message = "Stepping...".to_string();
            
            match self.language {
                Language::Basic => self.basic_interpreter.load_code(&self.code),
                Language::Pilot => self.pilot_interpreter.load_code(&self.code),
                Language::Logo => self.logo_interpreter.load_code(&self.code),
                Language::Pascal => self.pascal_interpreter.load_code(&self.code),
                Language::Forth => self.forth_interpreter.load_code(&self.code),
                Language::Prolog => self.prolog_interpreter.load_code(&self.code),
                Language::C => self.c_interpreter.load_code(&self.code),
            }
        }
        
        let (output, state) = match self.language {
            Language::Basic => self.basic_interpreter.step_execution(),
            Language::Pilot => self.pilot_interpreter.step_execution(),
            Language::Logo => self.logo_interpreter.step_execution(),
            Language::Pascal => self.pascal_interpreter.step_execution(),
            Language::Forth => self.forth_interpreter.step_execution(),
            Language::Prolog => self.prolog_interpreter.step_execution(),
            Language::C => self.c_interpreter.step_execution(),
        };
        
        self.output.push_str(&output);
        self.execution_state = state;
        self.update_draw_commands();
        
        if let ExecutionState::Finished = self.execution_state {
            self.status_message = "Execution finished.".to_string();
        } else {
            self.status_message = "Paused.".to_string();
            self.active_tab = Tab::Variables; // Show variables while stepping
        }
    }
    
    fn set_theme(&mut self, ctx: &egui::Context, theme: Theme) {
        self.theme = theme;
        match theme {
            Theme::Dark => ctx.set_visuals(egui::Visuals::dark()),
            Theme::Light => ctx.set_visuals(egui::Visuals::light()),
            Theme::Retro => {
                let mut visuals = egui::Visuals::dark();
                visuals.override_text_color = Some(egui::Color32::from_rgb(0, 255, 0));
                visuals.panel_fill = egui::Color32::from_rgb(10, 20, 10);
                ctx.set_visuals(visuals);
            }
        }
    }
}

impl eframe::App for TimeWarpApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if let ExecutionState::Running = self.execution_state {
            self.continue_execution();
            ctx.request_repaint();
        }

        if let Some(path) = self.file_dialog.show(ctx) {
            match self.file_dialog.mode {
                DialogMode::Open => {
                    if let Ok(content) = std::fs::read_to_string(&path) {
                        self.code = content;
                        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                            self.language = match ext.to_lowercase().as_str() {
                                "bas" | "basic" => Language::Basic,
                                "pilot" => Language::Pilot,
                                "logo" => Language::Logo,
                                "pas" | "pascal" => Language::Pascal,
                                "pl" | "pro" => Language::Prolog,
                                "f" | "fs" | "forth" => Language::Forth,
                                "c" | "h" => Language::C,
                                _ => self.language,
                            };
                        }
                    }
                }
                DialogMode::Save => {
                    let _ = std::fs::write(path, &self.code);
                }
            }
        }

        // Menu Bar
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open").clicked() {
                        self.file_dialog.open();
                        ui.close_menu();
                    }
                    if ui.button("Save").clicked() {
                        self.file_dialog.save();
                        ui.close_menu();
                    }
                    ui.separator();
                    if ui.button("Quit").clicked() {
                        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });
                
                ui.menu_button("Theme", |ui| {
                    if ui.button("Dark").clicked() { self.set_theme(ctx, Theme::Dark); ui.close_menu(); }
                    if ui.button("Light").clicked() { self.set_theme(ctx, Theme::Light); ui.close_menu(); }
                    if ui.button("Retro").clicked() { self.set_theme(ctx, Theme::Retro); ui.close_menu(); }
                });

                ui.menu_button("Examples", |ui| {
                    let dir_name = match self.language {
                        Language::Basic => "basic",
                        Language::Pilot => "pilot",
                        Language::Logo => "logo",
                        Language::Pascal => "pascal",
                        Language::Prolog => "prolog",
                        Language::Forth => "forth",
                        Language::C => "c",
                    };
                    
                    // Look for Examples folder in current directory or parent directories
                    let mut examples_path = std::path::PathBuf::from("Examples");
                    if !examples_path.exists() {
                        examples_path = std::path::PathBuf::from("../../Examples");
                    }
                    
                    let lang_path = examples_path.join(dir_name);
                    
                    if let Ok(entries) = std::fs::read_dir(lang_path) {
                        let mut files: Vec<_> = entries
                            .filter_map(|e| e.ok())
                            .map(|e| e.path())
                            .filter(|p| p.is_file())
                            .collect();
                        files.sort();
                        
                        for path in files {
                            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                                if ui.button(name).clicked() {
                                    if let Ok(content) = std::fs::read_to_string(&path) {
                                        self.code = content;
                                    }
                                    ui.close_menu();
                                }
                            }
                        }
                    } else {
                        ui.label("No examples found");
                    }
                });
                
                ui.separator();
                
                ui.label("Language:");
                egui::ComboBox::from_id_salt("lang_combo")
                    .selected_text(format!("{:?}", self.language))
                    .show_ui(ui, |ui| {
                        ui.selectable_value(&mut self.language, Language::Basic, "BASIC");
                        ui.selectable_value(&mut self.language, Language::Pilot, "PILOT");
                        ui.selectable_value(&mut self.language, Language::Logo, "Logo");
                        ui.selectable_value(&mut self.language, Language::Pascal, "Pascal");
                        ui.selectable_value(&mut self.language, Language::Prolog, "Prolog");
                        ui.selectable_value(&mut self.language, Language::Forth, "Forth");
                        ui.selectable_value(&mut self.language, Language::C, "C");
                    });
                
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if ui.button("▶ Run").clicked() {
                        self.run_code();
                    }
                    if ui.button("⏭ Step").clicked() {
                        self.step_code();
                    }
                });
            });
        });

        // Status Bar
        egui::TopBottomPanel::bottom("bottom_panel").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("Status:");
                ui.label(&self.status_message);
            });
        });

        // Main Content
        egui::CentralPanel::default().show(ctx, |ui| {
            let available_width = ui.available_width();
            let editor_width = available_width * 0.55; // 55% for editor
            
            egui::SidePanel::left("editor_panel")
                .resizable(true)
                .default_width(editor_width)
                .show_inside(ui, |ui| {
                    // Editor Area (Top)
                    egui::TopBottomPanel::top("editor_top")
                        .resizable(false)
                        .min_height(ui.available_height() * 0.85) // 85% height
                        .show_inside(ui, |ui| {
                            ui.heading("Editor");
                            
                            let language_name = format!("{:?}", self.language);
                            let mut layouter = |ui: &egui::Ui, string: &str, wrap_width: f32| {
                                let mut layout_job = syntax::highlight(ui.style(), string, &language_name);
                                layout_job.wrap.max_width = wrap_width;
                                ui.fonts(|f| f.layout_job(layout_job))
                            };

                            egui::ScrollArea::vertical().show(ui, |ui| {
                                ui.add(
                                    egui::TextEdit::multiline(&mut self.code)
                                        .code_editor()
                                        .lock_focus(true)
                                        .desired_width(f32::INFINITY)
                                        .desired_rows(30)
                                        .layouter(&mut layouter)
                                );
                            });
                        });

                    // Immediate Mode (Bottom)
                    egui::CentralPanel::default().show_inside(ui, |ui| {
                        ui.separator();
                        ui.heading("Immediate Mode");
                        ui.horizontal(|ui| {
                            ui.label(">");
                            let response = ui.text_edit_singleline(&mut self.immediate_code);
                            if ui.button("Run").clicked() || (response.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter))) {
                                self.run_immediate();
                            }
                        });
                    });
                });
            
            egui::CentralPanel::default().show_inside(ui, |ui| {
                ui.horizontal(|ui| {
                    ui.selectable_value(&mut self.active_tab, Tab::Output, "Output");
                    ui.selectable_value(&mut self.active_tab, Tab::Canvas, "Canvas");
                    ui.selectable_value(&mut self.active_tab, Tab::Variables, "Variables");
                    ui.selectable_value(&mut self.active_tab, Tab::Debug, "Debug");
                });
                ui.separator();
                
                match self.active_tab {
                    Tab::Output => {
                        egui::ScrollArea::vertical().show(ui, |ui| {
                            ui.add(
                                egui::TextEdit::multiline(&mut self.output)
                                    .desired_width(f32::INFINITY)
                                    .desired_rows(30)
                                    .font(egui::TextStyle::Monospace)
                            );
                            
                            let waiting_var = if let ExecutionState::WaitingForInput(ref var) = self.execution_state {
                                Some(var.clone())
                            } else {
                                None
                            };

                            if let Some(var) = waiting_var {
                                ui.separator();
                                ui.horizontal(|ui| {
                                    ui.label(format!("Input for {}: ", var));
                                    let response = ui.text_edit_singleline(&mut self.input_buffer);
                                    if ui.button("Submit").clicked() || (response.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter))) {
                                        self.submit_input();
                                    }
                                    // Auto-focus input
                                    if response.changed() {
                                        // keep focus
                                    }
                                });
                            }
                        });
                    }
                    Tab::Variables => {
                        let vars = match self.language {
                            Language::Basic => self.basic_interpreter.get_variables(),
                            Language::Pilot => self.pilot_interpreter.get_variables(),
                            Language::Logo => self.logo_interpreter.get_variables(),
                            Language::Pascal => self.pascal_interpreter.get_variables(),
                            Language::Forth => self.forth_interpreter.get_variables(),
                            Language::Prolog => self.prolog_interpreter.get_variables(),
                            Language::C => self.c_interpreter.get_variables(),
                        };
                        
                        egui::ScrollArea::vertical().show(ui, |ui| {
                            egui::Grid::new("vars_grid").striped(true).show(ui, |ui| {
                                ui.label("Variable");
                                ui.label("Value");
                                ui.end_row();
                                
                                for (k, v) in vars {
                                    ui.label(k);
                                    ui.label(v);
                                    ui.end_row();
                                }
                            });
                        });
                    }
                    Tab::Debug => {
                        ui.label("Debug features coming soon...");
                    }
                    Tab::Canvas => {
                        let (response, painter) = ui.allocate_painter(
                            ui.available_size(),
                            egui::Sense::hover(),
                        );
                        
                        // Draw background
                        let rect = response.rect;
                        painter.rect_filled(rect, 0.0, egui::Color32::from_rgb(20, 20, 20));
                        
                        // Center coordinate system (0,0 at center)
                        let center = rect.center();
                        
                        for cmd in &self.draw_commands {
                            match cmd {
                                DrawCommand::Clear { color } => {
                                    painter.rect_filled(rect, 0.0, egui::Color32::from_rgb(color.0, color.1, color.2));
                                }
                                DrawCommand::Line { x1, y1, x2, y2, color, width } => {
                                    // Transform coords: y is up (negative in screen coords), 0,0 is center
                                    let p1 = egui::pos2(center.x + x1, center.y - y1);
                                    let p2 = egui::pos2(center.x + x2, center.y - y2);
                                    painter.line_segment([p1, p2], egui::Stroke::new(*width, egui::Color32::from_rgb(color.0, color.1, color.2)));
                                }
                                DrawCommand::Turtle { x, y, angle, visible, color } => {
                                    if *visible {
                                        let tx = center.x + x;
                                        let ty = center.y - y;
                                        // Draw turtle as a triangle
                                        // Angle 0 is UP.
                                        // Points: Tip, Left Back, Right Back
                                        let tip_dist = 10.0;
                                        let back_dist = 8.0;
                                        // Logo: 0 is UP.
                                        // Math: 0 is Right.
                                        // So Logo 0 = Math 90. Logo 90 = Math 0.
                                        // Math Angle = 90 - Logo Angle.
                                        let math_rad = (90.0 - angle).to_radians();
                                        
                                        let tip = egui::pos2(
                                            tx + tip_dist * math_rad.cos(),
                                            ty - tip_dist * math_rad.sin() // Screen Y is down
                                        );
                                        
                                        let left_rad = (90.0 - angle + 140.0).to_radians();
                                        let right_rad = (90.0 - angle - 140.0).to_radians();
                                        
                                        let p_left = egui::pos2(
                                            tx + back_dist * left_rad.cos(),
                                            ty - back_dist * left_rad.sin()
                                        );
                                        let p_right = egui::pos2(
                                            tx + back_dist * right_rad.cos(),
                                            ty - back_dist * right_rad.sin()
                                        );
                                        
                                        painter.add(egui::Shape::convex_polygon(
                                            vec![tip, p_left, p_right],
                                            egui::Color32::from_rgb(color.0, color.1, color.2),
                                            egui::Stroke::NONE,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            });
        });
    }
}

fn setup_custom_fonts(ctx: &egui::Context) {
    let fonts = egui::FontDefinitions::default();
    // We could load custom fonts here if we had them
    ctx.set_fonts(fonts);
}
