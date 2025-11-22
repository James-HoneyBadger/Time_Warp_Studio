// SPDX-License-Identifier: MIT
// Copyright (c) 2025 James Temple <james@honey-badger.org>
//
// Time Warp IDE - Main Entry Point
//
// This file initializes the application, handles CLI arguments, and launches
// the egui-based GUI. It supports both GUI mode and CLI compilation mode.

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
use time_warp_unified::compiler::Compiler;

/// Main entry point for Time Warp IDE
///
/// Supports two modes:
/// - GUI mode: `time-warp` (default)
/// - CLI compilation mode: `time-warp --compile input.tw [-o output]`
fn main() -> Result<()> {
    // Initialize logging system
    tracing_subscriber::fmt::init();

    tracing::info!("Starting Time Warp Unified v{}", env!("CARGO_PKG_VERSION"));

    // Check for CLI compilation mode
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if !args.is_empty() && args[0] == "--compile" {
        return handle_cli_compilation(args);
    }

    // Launch GUI mode
    launch_gui()
}

/// Handle CLI compilation mode
///
/// Compiles a source file to a native executable
fn handle_cli_compilation(args: Vec<String>) -> Result<()> {
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
    
    let compiler = Compiler::new();
    compiler.compile_to_executable(&src, &out_path)?;
    println!("✅ Built executable: {}", out_path.display());
    Ok(())
}

/// Launch the GUI application
fn launch_gui() -> Result<()> {
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
            // Use egui default fonts for simplicity and consistency
            Ok(Box::new(TimeWarpApp::new(cc)))
        }),
    )
    .map_err(|e| anyhow::anyhow!("Failed to start application: {}", e))
}

/// Load application icon
///
/// Creates a 32x32 icon with blue-teal gradient and "TW" text
fn load_icon() -> egui::IconData {
    let mut rgba = vec![0u8; 32 * 32 * 4];

    for y in 0..32 {
        for x in 0..32 {
            let idx = (y * 32 + x) * 4;

            // Blue-teal gradient background
            let t = y as f32 / 32.0;
            let r = (30.0 * (1.0 - t) + 64.0 * t) as u8;
            let g = (144.0 * (1.0 - t) + 224.0 * t) as u8;
            let b = (255.0 * (1.0 - t) + 208.0 * t) as u8;

            // Draw "TW" pattern in white
            let is_tw =
                // T letter (left side)
                ((8..=10).contains(&y) && (6..=14).contains(&x)) ||  // Top bar
                ((10..=24).contains(&y) && (9..=11).contains(&x)) ||  // Vertical stroke
                // W letter (right side)
                ((8..=24).contains(&y) && (17..=18).contains(&x)) ||  // Left stroke
                ((20..=24).contains(&y) && (19..=21).contains(&x)) || // Middle dip
                ((8..=24).contains(&y) && (24..=25).contains(&x)); // Right stroke

            if is_tw {
                // White text
                rgba[idx] = 255;
                rgba[idx + 1] = 255;
                rgba[idx + 2] = 255;
                rgba[idx + 3] = 255;
            } else {
                // Gradient background
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
