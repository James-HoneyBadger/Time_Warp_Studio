use anyhow::Result;
use eframe::egui;

mod app;
mod graphics;
mod interpreter;
mod languages;
mod ui;
mod utils;

#[cfg(feature = "audio")]
mod audio;

#[cfg(feature = "ml")]
mod ml;

#[cfg(feature = "plugins")]
mod plugins;

mod game;
mod iot;

use app::TimeWarpApp;
use std::fs;
use std::path::PathBuf;
use time_warp_unified::compiler::TempleCodeCompiler;

fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    tracing::info!("Starting Time Warp Unified v{}", env!("CARGO_PKG_VERSION"));

    // Lightweight CLI: --compile <input> [-o <output>]
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if !args.is_empty() && args[0] == "--compile" {
        if args.len() < 2 {
            return Err(anyhow::anyhow!("Usage: --compile <input> [-o <output>]"));
        }
        let input = PathBuf::from(&args[1]);
        let mut output: Option<PathBuf> = None;
        if args.len() >= 4 && args[2] == "-o" {
            output = Some(PathBuf::from(&args[3]));
        }
        let src = fs::read_to_string(&input)?;
        let out_path = output.unwrap_or_else(|| {
            let mut p = input.clone();
            p.set_extension("");
            let stem = p.file_name().and_then(|s| s.to_str()).unwrap_or("a.out");
            let mut o = PathBuf::from(".");
            o.push(stem);
            o
        });
        let compiler = TempleCodeCompiler::new();
        compiler.compile_to_executable(&src, &out_path)?;
        println!("✅ Built executable: {}", out_path.display());
        return Ok(());
    }

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1400.0, 900.0])
            .with_min_inner_size([800.0, 600.0])
            .with_icon(load_icon()),
        ..Default::default()
    };

    eframe::run_native(
        "Time Warp IDE - Unified",
        options,
        Box::new(|cc| {
            // Don't configure custom fonts - use egui defaults
            // configure_fonts(&cc.egui_ctx);
            Ok(Box::new(TimeWarpApp::new(cc)))
        }),
    )
    .map_err(|e| anyhow::anyhow!("Failed to start application: {}", e))
}

fn load_icon() -> egui::IconData {
    // Simple 32x32 icon with Time Warp theme colors
    // Blue-teal gradient background with "TW" text representation
    let mut rgba = vec![0u8; 32 * 32 * 4];

    for y in 0..32 {
        for x in 0..32 {
            let idx = (y * 32 + x) * 4;

            // Create a simple gradient background (blue to teal)
            let t = y as f32 / 32.0;
            let r = (30.0 * (1.0 - t) + 64.0 * t) as u8;
            let g = (144.0 * (1.0 - t) + 224.0 * t) as u8;
            let b = (255.0 * (1.0 - t) + 208.0 * t) as u8;

            // Draw a simple "TW" pattern in the center
            let is_tw =
                // T letter (left side)
                ((8..=10).contains(&y) && (6..=14).contains(&x)) ||  // top bar
                ((10..=24).contains(&y) && (9..=11).contains(&x)) ||  // vertical
                // W letter (right side)
                ((8..=24).contains(&y) && (17..=18).contains(&x)) ||  // left stroke
                ((20..=24).contains(&y) && (19..=21).contains(&x)) || // middle dip
                ((8..=24).contains(&y) && (24..=25).contains(&x)); // right stroke

            if is_tw {
                rgba[idx] = 255; // white text
                rgba[idx + 1] = 255;
                rgba[idx + 2] = 255;
                rgba[idx + 3] = 255;
            } else {
                rgba[idx] = r;
                rgba[idx + 1] = g;
                rgba[idx + 2] = b;
                rgba[idx + 3] = 255;
            }
        }
    }

    egui::IconData {
        rgba,
        width: 32,
        height: 32,
    }
}

// Font configuration removed - using egui defaults
// Custom fonts can be added later if needed with embedded font data
