pub mod basic;
pub mod logo;
pub mod pilot;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Language {
    /// Unified Time Warp language (BASIC + PILOT + Logo)
    Unified,
    Pilot,
    Basic,
    Logo,
}

impl Language {
    pub fn from_extension(ext: &str) -> Self {
        match ext.to_lowercase().as_str() {
            // Unified language
            "tw" | "timewarp" => Language::Unified,
            "pilot" | "pil" => Language::Pilot,
            "bas" | "basic" => Language::Basic,
            "logo" | "lgo" => Language::Logo,
            _ => Language::Pilot,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Language::Unified => "Time Warp",
            Language::Pilot => "PILOT",
            Language::Basic => "BASIC",
            Language::Logo => "Logo",
        }
    }
}
