use std::path::{PathBuf};
use std::fs;

#[derive(PartialEq)]
pub enum DialogMode {
    Open,
    Save,
}

pub struct FileDialog {
    pub is_open: bool,
    pub mode: DialogMode,
    current_path: PathBuf,
    selected_file: Option<PathBuf>,
    input_filename: String,
    entries: Vec<PathBuf>,
}

impl FileDialog {
    pub fn new() -> Self {
        let current_path = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        let mut fd = Self {
            is_open: false,
            mode: DialogMode::Open,
            current_path,
            selected_file: None,
            input_filename: String::new(),
            entries: Vec::new(),
        };
        fd.refresh();
        fd
    }

    pub fn open(&mut self) {
        self.mode = DialogMode::Open;
        self.is_open = true;
        self.selected_file = None;
        self.input_filename.clear();
        self.refresh();
    }

    pub fn save(&mut self) {
        self.mode = DialogMode::Save;
        self.is_open = true;
        self.selected_file = None;
        self.input_filename.clear();
        self.refresh();
    }

    fn refresh(&mut self) {
        self.entries.clear();
        if let Ok(entries) = fs::read_dir(&self.current_path) {
            for entry in entries.flatten() {
                self.entries.push(entry.path());
            }
        }
        self.entries.sort_by(|a, b| {
            let a_dir = a.is_dir();
            let b_dir = b.is_dir();
            if a_dir && !b_dir {
                std::cmp::Ordering::Less
            } else if !a_dir && b_dir {
                std::cmp::Ordering::Greater
            } else {
                a.cmp(b)
            }
        });
    }

    pub fn show(&mut self, ctx: &egui::Context) -> Option<PathBuf> {
        let mut result = None;
        if self.is_open {
            egui::Window::new(match self.mode {
                DialogMode::Open => "Open File",
                DialogMode::Save => "Save File",
            })
            .collapsible(false)
            .resizable(true)
            .default_width(500.0)
            .default_height(400.0)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    if ui.button("⬆ Up").clicked() {
                        if let Some(parent) = self.current_path.parent() {
                            self.current_path = parent.to_path_buf();
                            self.refresh();
                        }
                    }
                    ui.label(self.current_path.to_string_lossy());
                });
                ui.separator();

                egui::ScrollArea::vertical().max_height(300.0).show(ui, |ui| {
                    let mut next_path = None;
                    for path in &self.entries {
                        let file_name = path.file_name().unwrap_or_default().to_string_lossy();
                        let is_dir = path.is_dir();
                        let icon = if is_dir { "📁" } else { "📄" };
                        
                        let text = format!("{} {}", icon, file_name);
                        let is_selected = Some(path.clone()) == self.selected_file;
                        
                        if ui.selectable_label(is_selected, text).clicked() {
                            if is_dir {
                                next_path = Some(path.clone());
                            } else {
                                self.selected_file = Some(path.clone());
                                if let DialogMode::Save = self.mode {
                                    self.input_filename = file_name.to_string();
                                }
                            }
                        }
                    }
                    
                    if let Some(path) = next_path {
                        self.current_path = path;
                        self.refresh();
                    }
                });

                ui.separator();
                
                if let DialogMode::Save = self.mode {
                    ui.horizontal(|ui| {
                        ui.label("Filename:");
                        ui.text_edit_singleline(&mut self.input_filename);
                    });
                }

                ui.horizontal(|ui| {
                    if ui.button("Cancel").clicked() {
                        self.is_open = false;
                    }
                    if ui.button(match self.mode {
                        DialogMode::Open => "Open",
                        DialogMode::Save => "Save",
                    }).clicked() {
                        match self.mode {
                            DialogMode::Open => {
                                if let Some(path) = &self.selected_file {
                                    result = Some(path.clone());
                                    self.is_open = false;
                                }
                            }
                            DialogMode::Save => {
                                if !self.input_filename.is_empty() {
                                    let path = self.current_path.join(&self.input_filename);
                                    result = Some(path);
                                    self.is_open = false;
                                }
                            }
                        }
                    }
                });
            });
        }
        result
    }
}
