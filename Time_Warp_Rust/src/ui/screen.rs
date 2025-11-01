use crate::app::TimeWarpApp;
use crate::interpreter::ScreenMode;
use eframe::egui;

/// Unified screen renderer: draws text and graphics in a single canvas based on current SCREEN mode
pub fn render(app: &mut TimeWarpApp, ui: &mut egui::Ui) {
    // Determine desired logical size
    let (desired_w, desired_h) = match app.interpreter.screen_mode {
        ScreenMode::Graphics { width, height } => (width as f32, height as f32),
        ScreenMode::Text { cols, rows } => {
            let char_w = 10.0f32;
            let char_h = 18.0f32;
            (cols as f32 * char_w, rows as f32 * char_h)
        }
    };

    // Allocate painter
    let desired = egui::vec2(desired_w, desired_h);
    let (response, painter) = ui.allocate_painter(desired, egui::Sense::hover());

    // Background
    match app.interpreter.screen_mode {
        ScreenMode::Graphics { .. } => {
            painter.rect_filled(response.rect, 0.0, app.turtle_state.bg_color);
        }
        ScreenMode::Text { .. } => {
            painter.rect_filled(response.rect, 0.0, app.current_theme.background());
        }
    }

    // Draw content based on mode
    match app.interpreter.screen_mode {
        ScreenMode::Graphics { .. } => {
            // World rect centered at (0,0) with size canvas_width x canvas_height
            let world = egui::Rect::from_center_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(
                    app.turtle_state.canvas_width,
                    app.turtle_state.canvas_height,
                ),
            );
            let to_screen = egui::emath::RectTransform::from_to(world, response.rect);

            // Lines
            for line in &app.turtle_state.lines {
                let p0 = to_screen * line.start;
                let p1 = to_screen * line.end;
                painter.line_segment([p0, p1], egui::Stroke::new(line.width, line.color));
            }
            // Turtle cursor
            if app.turtle_state.visible {
                let pos = to_screen * egui::pos2(app.turtle_state.x, app.turtle_state.y);
                let size = 8.0;
                painter.circle_filled(pos, size, app.current_theme.accent());
                let angle = app.turtle_state.heading.to_radians();
                let dir = egui::vec2(angle.sin(), -angle.cos()) * size * 1.5;
                painter.line_segment(
                    [pos, pos + dir],
                    egui::Stroke::new(2.0, app.current_theme.text()),
                );
            }
            // Optional overlay recent text output (last 10 lines)
            if app.show_overlay_text {
                let overlay_lines = 10usize;
                let start = app.interpreter.output.len().saturating_sub(overlay_lines);
                if start < app.interpreter.output.len() {
                    let margin = 8.0;
                    let mut y = response.rect.top() + margin;
                    for line in &app.interpreter.output[start..] {
                        let pos = egui::pos2(response.rect.left() + margin, y);
                        painter.text(
                            pos,
                            egui::Align2::LEFT_TOP,
                            line,
                            egui::TextStyle::Monospace.resolve(ui.style()),
                            app.current_theme.text(),
                        );
                        y += ui.text_style_height(&egui::TextStyle::Monospace);
                    }
                }
            }
        }
        ScreenMode::Text { cols: _, rows: _ } => {
            // Draw text buffer in a monospace grid
            let margin = 8.0;
            let mut y = response.rect.top() + margin;
            for line in &app.interpreter.text_lines {
                let pos = egui::pos2(response.rect.left() + margin, y);
                painter.text(
                    pos,
                    egui::Align2::LEFT_TOP,
                    line,
                    egui::TextStyle::Monospace.resolve(ui.style()),
                    app.current_theme.text(),
                );
                y += ui.text_style_height(&egui::TextStyle::Monospace);
                if y > response.rect.bottom() - margin {
                    break;
                }
            }
        }
    }
}
