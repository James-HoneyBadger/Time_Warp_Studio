use eframe::egui;
use std::collections::HashMap;

use crate::graphics::TurtleState;
use crate::interpreter::Interpreter;
use crate::ui::themes::Theme;

/// Main application state for Time Warp IDE
///
/// Manages the entire IDE lifecycle including:
/// - Multi-file editing with undo/redo support
/// - Program execution across PILOT, BASIC, and Logo
/// - Turtle graphics visualization with zoom/pan
/// - Theme management and UI customization
/// - Input/output handling for interactive programs
///
/// # Architecture
/// - File buffers: In-memory text storage for open files
/// - Interpreter: Stateless command processor for all languages
/// - Turtle state: Separate graphics state for drawing
/// - UI state: Tab management, find/replace, debug mode
///
/// # Example
/// ```ignore
/// let app = TimeWarpApp::default();
/// eframe::run_native("Time Warp", options, Box::new(|_| Ok(Box::new(app))));
/// ```
pub struct TimeWarpApp {
    // File management
    pub file_buffers: HashMap<String, String>,
    pub file_modified: HashMap<String, bool>,
    pub open_files: Vec<String>,
    pub current_file_index: usize,
    pub last_file_path: Option<String>,
    #[allow(dead_code)]
    pub file_tree: Vec<String>,

    // UI state
    pub active_tab: usize, // 0 = Editor, 1 = Output & Graphics, 2 = Debug, 3 = Explorer, 4 = Help
    pub show_find_replace: bool,
    pub find_text: String,
    pub replace_text: String,
    pub current_theme: Theme,

    // Execution state
    pub interpreter: Interpreter,
    pub is_executing: bool,
    pub error_message: Option<String>,

    // Edit history (future features)
    pub undo_history: Vec<String>,
    pub undo_position: usize,
    pub max_undo_steps: usize,

    // Graphics
    pub turtle_state: TurtleState,
    pub turtle_zoom: f32,
    #[allow(dead_code)]
    pub turtle_pan: egui::Vec2,

    // Input prompt state
    pub input_buffer: String,

    // Keyboard state for INKEY$
    pub last_key_pressed: Option<String>,

    // UI options
    pub show_overlay_text: bool,
    pub show_about_dialog: bool,

    // Debug state (future features)
    pub debug_mode: bool,
    #[allow(dead_code)]
    pub breakpoints: HashMap<String, Vec<usize>>,
    pub current_debug_line: Option<usize>,
    pub step_mode: bool,
}

impl TimeWarpApp {
    pub fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        Self {
            file_buffers: HashMap::new(),
            file_modified: HashMap::new(),
            open_files: vec!["untitled.pilot".to_string()],
            current_file_index: 0,
            last_file_path: None,
            file_tree: Vec::new(),

            active_tab: 0,
            show_find_replace: false,
            find_text: String::new(),
            replace_text: String::new(),
            current_theme: Theme::default(),

            interpreter: Interpreter::new(),
            is_executing: false,
            error_message: None,

            undo_history: Vec::new(),
            undo_position: 0,
            max_undo_steps: 100,

            turtle_state: TurtleState::new(),
            turtle_zoom: 1.0,
            turtle_pan: egui::Vec2::ZERO,

            input_buffer: String::new(),
            last_key_pressed: None,

            show_overlay_text: true,
            show_about_dialog: false,

            debug_mode: false,
            breakpoints: HashMap::new(),
            current_debug_line: None,
            step_mode: false,
        }
    }

    pub fn current_file(&self) -> Option<&String> {
        self.open_files.get(self.current_file_index)
    }

    pub fn current_code(&self) -> String {
        self.current_file()
            .and_then(|f| self.file_buffers.get(f))
            .cloned()
            .unwrap_or_default()
    }

    pub fn set_current_code(&mut self, code: String) {
        if let Some(file) = self.current_file().cloned() {
            // Save to undo history before changing
            let old_code = self.file_buffers.get(&file).cloned().unwrap_or_default();
            if old_code != code {
                self.push_undo_state(old_code);
            }
            self.file_buffers.insert(file.clone(), code);
            self.file_modified.insert(file, true);
        }
    }

    pub fn push_undo_state(&mut self, state: String) {
        // Remove any states after current position
        self.undo_history.truncate(self.undo_position);

        // Add new state
        self.undo_history.push(state);

        // Maintain max history size
        if self.undo_history.len() > self.max_undo_steps {
            self.undo_history.remove(0);
        } else {
            self.undo_position = self.undo_history.len();
        }
    }

    pub fn undo(&mut self) {
        if self.undo_position > 0 {
            self.undo_position -= 1;
            if let Some(state) = self.undo_history.get(self.undo_position).cloned() {
                if let Some(file) = self.current_file().cloned() {
                    self.file_buffers.insert(file.clone(), state);
                    self.file_modified.insert(file, true);
                }
            }
        }
    }

    pub fn redo(&mut self) {
        if self.undo_position < self.undo_history.len() {
            if let Some(state) = self.undo_history.get(self.undo_position).cloned() {
                if let Some(file) = self.current_file().cloned() {
                    self.file_buffers.insert(file.clone(), state);
                    self.file_modified.insert(file, true);
                }
                self.undo_position += 1;
            }
        }
    }
}

impl eframe::App for TimeWarpApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Capture keyboard input for INKEY$
        ctx.input(|i| {
            // Check for any key events
            for event in &i.events {
                if let egui::Event::Key {
                    key, pressed: true, ..
                } = event
                {
                    // Convert key to string representation
                    self.last_key_pressed = Some(format!("{:?}", key));
                }
                // Also capture text input for printable characters
                if let egui::Event::Text(text) = event {
                    if !text.is_empty() {
                        self.last_key_pressed = Some(text.clone());
                    }
                }
            }
        });

        // Apply theme
        self.current_theme.apply(ctx);

        // Top menu bar
        crate::ui::menubar::render(self, ctx);

        // Main content area
        egui::CentralPanel::default().show(ctx, |ui| {
            // Tab bar
            crate::ui::editor::render_tab_bar(self, ui);

            ui.separator();

            // Content based on active tab
            match self.active_tab {
                0 => crate::ui::editor::render(self, ui),
                1 => crate::ui::output::render(self, ui),
                2 => crate::ui::debugger::render(self, ui),
                3 => crate::ui::explorer::render(self, ui),
                4 => crate::ui::help::render(self, ui),
                _ => {}
            }
        });

        // Status bar
        crate::ui::statusbar::render(self, ctx);

        // Find/replace dialog
        if self.show_find_replace {
            crate::ui::editor::render_find_replace(self, ctx);
        }

        // Error notification
        if let Some(ref msg) = self.error_message.clone() {
            egui::Window::new("Error")
                .collapsible(false)
                .resizable(false)
                .show(ctx, |ui| {
                    ui.colored_label(egui::Color32::RED, msg);
                    if ui.button("OK").clicked() {
                        self.error_message = None;
                    }
                });
        }

        // About dialog
        if self.show_about_dialog {
            egui::Window::new("About Time Warp IDE")
                .collapsible(false)
                .resizable(false)
                .show(ctx, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.heading("Time Warp IDE");
                        ui.label(format!("Version {}", env!("CARGO_PKG_VERSION")));
                        ui.add_space(10.0);
                        ui.label("Educational programming environment for TempleCode");
                        ui.label("Combining BASIC, PILOT, and Logo");
                        ui.add_space(10.0);
                        ui.hyperlink_to(
                            "GitHub Repository",
                            "https://github.com/James-HoneyBadger/Time_Warp",
                        );
                        ui.add_space(10.0);
                        ui.label("© 2025 James Temple");
                        ui.add_space(10.0);
                        if ui.button("Close").clicked() {
                            self.show_about_dialog = false;
                        }
                    });
                });
        }
    }
}
