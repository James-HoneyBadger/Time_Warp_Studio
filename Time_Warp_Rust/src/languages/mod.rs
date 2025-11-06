pub mod basic;
pub mod logo;
pub mod pilot;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Language {
    /// Unified TempleCode language (BASIC + PILOT + Logo)
    TempleCode,
    Pilot,
    Basic,
    Logo,
}

impl Language {
    pub fn from_extension(ext: &str) -> Self {
        match ext.to_lowercase().as_str() {
            // TempleCode unified language
            "tc" | "temple" | "templecode" => Language::TempleCode,
            "pilot" | "pil" => Language::Pilot,
            "bas" | "basic" => Language::Basic,
            "logo" | "lgo" => Language::Logo,
            _ => Language::Pilot,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Language::TempleCode => "TempleCode",
            Language::Pilot => "PILOT",
            Language::Basic => "BASIC",
            Language::Logo => "Logo",
        }
    }
}
