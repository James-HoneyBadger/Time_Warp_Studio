use eframe::egui;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Theme {
    AmberPhosphor,
    GreenPhosphor,
    BluePhosphor,
    #[default]
    ModernDark,
    ModernLight,
    Dracula,
    Monokai,
    SolarizedDark,
}

impl Theme {
    pub fn all() -> Vec<Theme> {
        vec![
            Theme::AmberPhosphor,
            Theme::GreenPhosphor,
            Theme::BluePhosphor,
            Theme::ModernDark,
            Theme::ModernLight,
            Theme::Dracula,
            Theme::Monokai,
            Theme::SolarizedDark,
        ]
    }

    pub fn name(&self) -> &str {
        match self {
            Theme::AmberPhosphor => "Amber Phosphor",
            Theme::GreenPhosphor => "Green Phosphor",
            Theme::BluePhosphor => "Blue Phosphor",
            Theme::ModernDark => "Modern Dark",
            Theme::ModernLight => "Modern Light",
            Theme::Dracula => "Dracula",
            Theme::Monokai => "Monokai",
            Theme::SolarizedDark => "Solarized Dark",
        }
    }

    pub fn background(&self) -> egui::Color32 {
        match self {
            Theme::AmberPhosphor => egui::Color32::from_rgb(25, 20, 12),
            Theme::GreenPhosphor => egui::Color32::from_rgb(12, 20, 12),
            Theme::BluePhosphor => egui::Color32::from_rgb(10, 15, 25),
            Theme::ModernDark => egui::Color32::from_rgb(30, 30, 35),
            Theme::ModernLight => egui::Color32::from_rgb(250, 250, 252),
            Theme::Dracula => egui::Color32::from_rgb(40, 42, 54),
            Theme::Monokai => egui::Color32::from_rgb(39, 40, 34),
            Theme::SolarizedDark => egui::Color32::from_rgb(0, 43, 54),
        }
    }

    pub fn text(&self) -> egui::Color32 {
        match self {
            Theme::AmberPhosphor => egui::Color32::from_rgb(255, 176, 0),
            Theme::GreenPhosphor => egui::Color32::from_rgb(51, 255, 51),
            Theme::BluePhosphor => egui::Color32::from_rgb(100, 200, 255),
            Theme::ModernDark => egui::Color32::from_rgb(220, 220, 220),
            Theme::ModernLight => egui::Color32::from_rgb(30, 30, 30),
            Theme::Dracula => egui::Color32::from_rgb(248, 248, 242),
            Theme::Monokai => egui::Color32::from_rgb(248, 248, 240),
            Theme::SolarizedDark => egui::Color32::from_rgb(131, 148, 150),
        }
    }

    pub fn accent(&self) -> egui::Color32 {
        match self {
            Theme::AmberPhosphor => egui::Color32::from_rgb(255, 200, 100),
            Theme::GreenPhosphor => egui::Color32::from_rgb(100, 255, 100),
            Theme::BluePhosphor => egui::Color32::from_rgb(150, 220, 255),
            Theme::ModernDark => egui::Color32::from_rgb(100, 150, 255),
            Theme::ModernLight => egui::Color32::from_rgb(0, 100, 200),
            Theme::Dracula => egui::Color32::from_rgb(139, 233, 253),
            Theme::Monokai => egui::Color32::from_rgb(102, 217, 239),
            Theme::SolarizedDark => egui::Color32::from_rgb(38, 139, 210),
        }
    }

    pub fn panel(&self) -> egui::Color32 {
        match self {
            Theme::AmberPhosphor => egui::Color32::from_rgb(30, 25, 15),
            Theme::GreenPhosphor => egui::Color32::from_rgb(15, 25, 15),
            Theme::BluePhosphor => egui::Color32::from_rgb(15, 20, 30),
            Theme::ModernDark => egui::Color32::from_rgb(40, 40, 45),
            Theme::ModernLight => egui::Color32::from_rgb(255, 255, 255),
            Theme::Dracula => egui::Color32::from_rgb(68, 71, 90),
            Theme::Monokai => egui::Color32::from_rgb(49, 50, 44),
            Theme::SolarizedDark => egui::Color32::from_rgb(7, 54, 66),
        }
    }

    pub fn apply(&self, ctx: &egui::Context) {
        let mut style = (*ctx.style()).clone();

        style.visuals.override_text_color = Some(self.text());
        style.visuals.extreme_bg_color = self.background();
        style.visuals.faint_bg_color = self.panel();
        style.visuals.selection.bg_fill = self.accent().linear_multiply(0.3);
        style.visuals.selection.stroke = egui::Stroke::new(1.0, self.accent());

        ctx.set_style(style);
    }
}

// Default is derived; ModernDark marked as the default variant
