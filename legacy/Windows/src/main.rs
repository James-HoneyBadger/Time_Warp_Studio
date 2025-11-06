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
            code: String::from("REM Time Warp v3.0.0 (Windows)\nPRINT \"Hello from Rust IDE\"\n"),
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

    fn load_example(&mut self) {
        if let Some(path) = self.examples.get(self.selected_example) {
            if let Ok(s) = fs::read_to_string(path) {
                self.code = s;
                self.status = format!(
                    "Loaded example: {}",
                    path.file_name().and_then(|n| n.to_str()).unwrap_or("")
                );
            }
        }
    }

    fn clear_console(&mut self) {
        if let Ok(mut buf) = self.console.lock() {
            buf.clear();
        }
    }

    // removed unused append_console helper to avoid warnings

    fn run_program(&mut self, ctx: &egui::Context) {
        if self.running {
            return;
        }
        self.clear_console();
        self.status = String::from("Running...");
        self.running = true;
        let code = self.code.clone();
        let console = Arc::clone(&self.console);
        let turtle = Arc::clone(&self.turtle);

        let handle = thread::spawn(move || {
            // Run with in-process Rust interpreter
            let console_adapter = BufferConsole::new(Arc::clone(&console));
            let turtle_adapter = SharedTurtle::new(Arc::clone(&turtle));
            let mut interp = Interpreter::new(console_adapter, turtle_adapter);
            if let Err(e) = interp.run(&code) {
                if let Ok(mut c) = console.lock() {
                    c.push_str(&format!("Error: {}\n", e));
                }
            }
        });

        // Store handle and request repaints until finished
        self.runner_handle = Some(handle);
        let repaint = ctx.clone();
        thread::spawn(move || {
            // simple timer to keep UI fresh
            for _ in 0..200 {
                // ~10s at 50ms
                thread::sleep(std::time::Duration::from_millis(50));
                repaint.request_repaint();
            }
        });
    }

    fn poll_runner(&mut self) {
        if let Some(h) = &self.runner_handle {
            if h.is_finished() {
                // join and clear handle
                let h = self.runner_handle.take().unwrap();
                let _ = h.join();
                self.running = false;
                self.status = String::from("Program completed");
            }
        }
    }

    fn export_png(&mut self, path: &str) {
        // Use the interpreter's EXPORTPNG on current turtle state
        let console = Arc::clone(&self.console);
        let turtle = Arc::clone(&self.turtle);
        let console_adapter = BufferConsole::new(Arc::clone(&console));
        let turtle_adapter = SharedTurtle::new(Arc::clone(&turtle));
        let mut interp = Interpreter::new(console_adapter, turtle_adapter);
        let safe_path = path.to_string();
        let program = format!("EXPORTPNG \"{}\"", safe_path);
        if let Err(e) = interp.run(&program) {
            if let Ok(mut c) = console.lock() {
                c.push_str(&format!("Error exporting PNG: {}\n", e));
            }
        }
    }

    fn ensure_exports_dir(&self) -> PathBuf {
        let p = self.project_root.join("exports");
        let _ = std::fs::create_dir_all(&p);
        p
    }

    fn show_toast<S: Into<String>>(&mut self, msg: S) {
        self.toast = Some((msg.into(), Instant::now()));
    }

    fn get_default_save_dir(&self) -> PathBuf {
        if let Some(d) = &self.last_save_dir {
            if d.is_dir() {
                return d.clone();
            }
        }
        self.ensure_exports_dir()
    }

    fn persist_last_save_dir<P: AsRef<Path>>(&self, dir: P) {
        let path = self.project_root.join(".time_warp_last_save_dir");
        let _ = fs::write(path, dir.as_ref().to_string_lossy().as_bytes());
    }

    fn record_save_path<P: AsRef<Path>>(&mut self, file_path: P) {
        if let Some(parent) = file_path.as_ref().parent() {
            self.last_save_dir = Some(parent.to_path_buf());
            self.persist_last_save_dir(parent);
        }
    }

    fn open_exports_dir(&self) {
        let dir = self.ensure_exports_dir();
        #[cfg(target_os = "linux")]
        let cmd = ("xdg-open", vec![dir.to_string_lossy().to_string()]);
        #[cfg(target_os = "macos")]
        let cmd = ("open", vec![dir.to_string_lossy().to_string()]);
        #[cfg(target_os = "windows")]
        let cmd = (
            "cmd",
            vec![
                "/C".into(),
                "start".into(),
                dir.to_string_lossy().to_string(),
            ],
        );
        let _ = std::process::Command::new(cmd.0).args(cmd.1).spawn();
    }

    fn open_last_save_dir(&self) {
        let dir = self.get_default_save_dir();
        #[cfg(target_os = "linux")]
        let cmd = ("xdg-open", vec![dir.to_string_lossy().to_string()]);
        #[cfg(target_os = "macos")]
        let cmd = ("open", vec![dir.to_string_lossy().to_string()]);
        #[cfg(target_os = "windows")]
        let cmd = (
            "cmd",
            vec![
                "/C".into(),
                "start".into(),
                dir.to_string_lossy().to_string(),
            ],
        );
        let _ = std::process::Command::new(cmd.0).args(cmd.1).spawn();
    }

    fn quick_save_png(&mut self) {
        let dir = self.get_default_save_dir();
        // Ensure directory exists just in case
        let _ = std::fs::create_dir_all(&dir);
        let epoch = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        let filename = format!("time_warp_{}.png", epoch);
        let path = dir.join(&filename);
        if let Some(p) = path.to_str() {
            self.export_png(p);
            self.record_save_path(&path);
            self.status = format!("Saved PNG: {}", filename);
            self.show_toast(self.status.clone());
        }
    }
}

impl eframe::App for AppState {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.poll_runner();

        // Toast overlay (auto-hide)
        let mut clear_toast = false;
        if let Some((msg, started)) = &self.toast {
            if started.elapsed().as_secs_f32() < 2.5 {
                egui::Area::new("toast_area")
                    .anchor(egui::Align2::RIGHT_TOP, egui::vec2(-12.0, 12.0))
                    .order(egui::Order::Foreground)
                    .show(ctx, |ui| {
                        egui::Frame::none()
                            .fill(egui::Color32::from_black_alpha(220))
                            .rounding(5.0)
                            .inner_margin(egui::Margin::symmetric(12.0, 8.0))
                            .show(ui, |ui| {
                                ui.label(
                                    egui::RichText::new(msg.clone()).color(egui::Color32::WHITE),
                                );
                            });
                    });
            } else {
                clear_toast = true;
            }
        }
        if clear_toast {
            self.toast = None;
        }

        // Menu bar at top
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Run\tCtrl+R").clicked() {
                        self.run_program(ctx);
                        ui.close_menu();
                    }
                    if ui.button("Quick Save PNG").clicked() {
                        self.quick_save_png();
                        ui.close_menu();
                    }
                    if ui.button("Save PNG...\tCtrl+S").clicked() {
                        let default_dir = self.get_default_save_dir();
                        if let Some(path) = FileDialog::new()
                            .set_title("Save canvas as PNG")
                            .add_filter("PNG Image", &["png"])
                            .set_file_name("time_warp.png")
                            .set_directory(default_dir)
                            .save_file()
                        {
                            if let Some(p) = path.to_str() {
                                self.export_png(p);
                            }
                            if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                                self.status = format!("Saved PNG: {}", name);
                                self.show_toast(format!("Saved PNG: {}", name));
                            }
                            self.record_save_path(&path);
                            ui.close_menu();
                        }
                    }
                    if ui.button("Open last save folder").clicked() {
                        self.open_last_save_dir();
                        ui.close_menu();
                    }
                    if ui.button("Open exports folder").clicked() {
                        self.open_exports_dir();
                        ui.close_menu();
                    }
                    ui.separator();
                    if ui.button("Reset Canvas").clicked() {
                        if let Ok(mut m) = self.turtle.lock() {
                            m.clear();
                        }
                        ui.close_menu();
                    }
                    if ui.button("Clear Console").clicked() {
                        self.clear_console();
                        ui.close_menu();
                    }
                });
                let mut chosen_example_top: Option<usize> = None;
                ui.menu_button("Examples", |ui| {
                    if self.examples.is_empty() {
                        ui.label("(none)");
                    } else {
                        let names: Vec<(usize, String)> = self
                            .examples
                            .iter()
                            .enumerate()
                            .map(|(i, p)| {
                                let name = p
                                    .file_name()
                                    .and_then(|s| s.to_str())
                                    .unwrap_or("")
                                    .to_string();
                                (i, name)
                            })
                            .collect();
                        for (i, name) in names {
                            if ui.button(name).clicked() {
                                chosen_example_top = Some(i);
                                ui.close_menu();
                            }
                        }
                    }
                });
                if let Some(i) = chosen_example_top {
                    self.selected_example = i;
                    self.load_example();
                }
                ui.menu_button("Help", |ui| {
                    if ui.button("About...").clicked() {
                        self.show_about = true;
                        ui.close_menu();
                    }
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical(|ui| {
                // Controls + editor
                ui.horizontal(|ui| {
                    if ui.button("Run (Ctrl+R)").clicked() {
                        self.run_program(ctx);
                    }
                    if ui.button("Clear Console").clicked() {
                        self.clear_console();
                    }
                    if ui.button("Reset Canvas").clicked() {
                        if let Ok(mut m) = self.turtle.lock() {
                            m.clear();
                        }
                    }
                    if ui.button("Quick Save").clicked() {
                        self.quick_save_png();
                    }
                    if ui.button("Save PNG...").clicked() {
                        let default_dir = self.get_default_save_dir();
                        if let Some(path) = FileDialog::new()
                            .set_title("Save canvas as PNG")
                            .add_filter("PNG Image", &["png"])
                            .set_file_name("time_warp.png")
                            .set_directory(default_dir)
                            .save_file()
                        {
                            if let Some(p) = path.to_str() {
                                self.export_png(p);
                                if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                                    self.status = format!("Saved PNG: {}", name);
                                    self.show_toast(format!("Saved PNG: {}", name));
                                }
                            }
                            self.record_save_path(&path);
                        }
                    }
                    ui.label(format!("Status: {}", self.status));
                });
                ui.add(
                    egui::TextEdit::multiline(&mut self.code)
                        .font(egui::TextStyle::Monospace)
                        .desired_rows(10),
                );
                ui.separator();
                // Canvas
                ui.heading("Canvas");
                let rect = ui.available_rect_before_wrap();
                let (resp, painter) = ui.allocate_painter(rect.size(), egui::Sense::hover());
                let top_left = resp.rect.left_top();
                let model = self.turtle.lock().unwrap().clone();
                painter.rect_filled(resp.rect, 0.0, egui::Color32::BLACK);
                let sx = resp.rect.width() / model.width.max(1.0);
                let sy = resp.rect.height() / model.height.max(1.0);
                for seg in &model.lines {
                    let to_screen = |x: f32, y: f32| -> egui::Pos2 {
                        egui::pos2(top_left.x + x * sx, top_left.y + y * sy)
                    };
                    let col = egui::Color32::from_rgb(seg.color.0, seg.color.1, seg.color.2);
                    painter.line_segment(
                        [to_screen(seg.x1, seg.y1), to_screen(seg.x2, seg.y2)],
                        egui::Stroke::new(1.5, col),
                    );
                }

                ui.separator();
                // Console
                ui.heading("Console");
                let console_text = self.console.lock().unwrap().clone();
                egui::ScrollArea::vertical()
                    .max_height(160.0)
                    .show(ui, |ui| {
                        ui.label(egui::RichText::new(console_text).monospace());
                    });
                ui.separator();
                ui.horizontal(|ui| {
                    ui.label("Examples:");
                    if self.examples.is_empty() {
                        ui.label("(none found)");
                    } else {
                        let names: Vec<String> = self
                            .examples
                            .iter()
                            .map(|p| {
                                p.file_name()
                                    .and_then(|s| s.to_str())
                                    .unwrap_or("")
                                    .to_string()
                            })
                            .collect();
                        egui::ComboBox::from_label("")
                            .selected_text(
                                names
                                    .get(self.selected_example)
                                    .cloned()
                                    .unwrap_or_default(),
                            )
                            .show_ui(ui, |ui| {
                                for (i, n) in names.iter().enumerate() {
                                    ui.selectable_value(&mut self.selected_example, i, n);
                                }
                            });
                        if ui.button("Load").clicked() {
                            self.load_example();
                        }
                    }
                });
            });
        });

        // Shortcuts
        if ctx.input(|i| {
            i.modifiers.matches_logically(egui::Modifiers::CTRL) && i.key_pressed(egui::Key::R)
        }) {
            self.run_program(ctx);
        }

        if ctx.input(|i| {
            i.modifiers.matches_logically(egui::Modifiers::CTRL) && i.key_pressed(egui::Key::S)
        }) {
            let default_dir = self.get_default_save_dir();
            if let Some(path) = FileDialog::new()
                .set_title("Save canvas as PNG")
                .add_filter("PNG Image", &["png"])
                .set_file_name("time_warp.png")
                .set_directory(default_dir)
                .save_file()
            {
                if let Some(p) = path.to_str() {
                    self.export_png(p);
                    if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                        self.status = format!("Saved PNG: {}", name);
                        self.show_toast(format!("Saved PNG: {}", name));
                    }
                }
                self.record_save_path(&path);
            }
        }

        // About dialog
        if self.show_about {
            egui::Window::new("About Time Warp")
                .collapsible(false)
                .resizable(false)
                .open(&mut self.show_about)
                .show(ctx, |ui| {
                    ui.heading("Time Warp v3.0.0 (Windows)");
                    ui.label("A lightweight TempleCode IDE using egui/eframe.");
                    ui.separator();
                    ui.label("Shortcuts:");
                    ui.monospace("Ctrl+R — Run\nCtrl+S — Save PNG");
                    ui.separator();
                    ui.label("© 2025");
                });
        }
    }
}

fn main() -> eframe::Result<()> {
    // Determine project root as parent of the Rust dir
    let exe_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let project_root = exe_dir.parent().unwrap_or(&exe_dir).to_path_buf();

    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size(Vec2::new(1024.0, 720.0)),
        ..Default::default()
    };
    eframe::run_native(
        "Time Warp v3.0.0 (Windows)",
        native_options,
        Box::new(move |_cc| Box::new(AppState::new(project_root))),
    )
}
