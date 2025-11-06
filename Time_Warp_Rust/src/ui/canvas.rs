use crate::app::TimeWarpApp;
use eframe::egui;

/// Legacy canvas render function - DEPRECATED
/// Use crate::ui::screen::render() for unified screen rendering instead
#[allow(dead_code)]
pub fn render_canvas(app: &mut TimeWarpApp, ui: &mut egui::Ui) {
    ui.horizontal(|ui| {
        ui.heading("Turtle Graphics");
        ui.separator();
        if ui.button("+ Zoom").clicked() {
            app.turtle_zoom = (app.turtle_zoom * 1.2).clamp(0.1, 10.0);
        }
        if ui.button("- Zoom").clicked() {
            app.turtle_zoom = (app.turtle_zoom / 1.2).clamp(0.1, 10.0);
        }
        if ui.button("Reset View").clicked() {
            app.turtle_zoom = 1.0;
            app.turtle_pan = egui::Vec2::ZERO;
        }
        ui.separator();
        ui.label(format!("Zoom: {:.2}x", app.turtle_zoom));
        ui.label(format!(
            "Pan: ({:.0}, {:.0})",
            app.turtle_pan.x, app.turtle_pan.y
        ));
        ui.label(format!(
            "Pos: ({:.0}, {:.0})",
            app.turtle_state.x, app.turtle_state.y
        ));
        ui.label(format!("Heading: {:.0}°", app.turtle_state.heading));
    });

    ui.separator();

    // Interactive painter
    let desired = egui::Vec2::new(
        app.turtle_state.canvas_width,
        app.turtle_state.canvas_height,
    );
    let (response, painter) = ui.allocate_painter(desired, egui::Sense::drag());

    // Mouse wheel zoom (when hovered)
    if response.hovered() {
        let scroll = ui.input(|i| i.smooth_scroll_delta.y + i.raw_scroll_delta.y);
        if scroll.abs() > 0.0 {
            let factor = if scroll > 0.0 { 1.1 } else { 1.0 / 1.1 };
            app.turtle_zoom = (app.turtle_zoom * factor).clamp(0.1, 10.0);
        }
    }

    // Drag to pan (convert screen delta to world delta)
    if response.dragged() {
        let delta = response.drag_delta();
        let scale_x = (app.turtle_state.canvas_width / app.turtle_zoom) / response.rect.width();
        let scale_y = (app.turtle_state.canvas_height / app.turtle_zoom) / response.rect.height();
        app.turtle_pan.x -= delta.x * scale_x;
        app.turtle_pan.y -= delta.y * scale_y;
    }

    // World-to-screen transform with pan and zoom
    let world_size = egui::vec2(
        app.turtle_state.canvas_width / app.turtle_zoom,
        app.turtle_state.canvas_height / app.turtle_zoom,
    );
    let world_center = egui::pos2(-app.turtle_pan.x, -app.turtle_pan.y);
    let to_screen = egui::emath::RectTransform::from_to(
        egui::Rect::from_center_size(world_center, world_size),
        response.rect,
    );

    // Background
    painter.rect_filled(response.rect, 0.0, app.turtle_state.bg_color);

    // Grid
    let grid_spacing = 50.0;
    let visible_cols = ((app.turtle_state.canvas_width / grid_spacing) as i32) + 2;
    let visible_rows = ((app.turtle_state.canvas_height / grid_spacing) as i32) + 2;
    for i in -visible_cols..=visible_cols {
        let x = i as f32 * grid_spacing;
        let start = to_screen * egui::pos2(x, -app.turtle_state.canvas_height);
        let end = to_screen * egui::pos2(x, app.turtle_state.canvas_height);
        painter.line_segment(
            [start, end],
            egui::Stroke::new(0.5, egui::Color32::from_gray(40)),
        );
    }
    for j in -visible_rows..=visible_rows {
        let y = j as f32 * grid_spacing;
        let start = to_screen * egui::pos2(-app.turtle_state.canvas_width, y);
        let end = to_screen * egui::pos2(app.turtle_state.canvas_width, y);
        painter.line_segment(
            [start, end],
            egui::Stroke::new(0.5, egui::Color32::from_gray(40)),
        );
    }

    // Axes
    let x0 = to_screen * egui::pos2(-app.turtle_state.canvas_width, 0.0);
    let x1 = to_screen * egui::pos2(app.turtle_state.canvas_width, 0.0);
    let y0 = to_screen * egui::pos2(0.0, -app.turtle_state.canvas_height);
    let y1 = to_screen * egui::pos2(0.0, app.turtle_state.canvas_height);
    painter.line_segment(
        [x0, x1],
        egui::Stroke::new(1.0, egui::Color32::from_gray(80)),
    );
    painter.line_segment(
        [y0, y1],
        egui::Stroke::new(1.0, egui::Color32::from_gray(80)),
    );

    // Draw lines
    for line in &app.turtle_state.lines {
        let start = to_screen * line.start;
        let end = to_screen * line.end;
        painter.line_segment(
            [start, end],
            egui::Stroke::new(line.width * app.turtle_zoom, line.color),
        );
    }

    // Draw turtle cursor
    if app.turtle_state.visible {
        let pos = to_screen * egui::pos2(app.turtle_state.x, app.turtle_state.y);
        let size = 10.0 * app.turtle_zoom;
        painter.circle_filled(pos, size, app.current_theme.accent());
        let angle = app.turtle_state.heading.to_radians();
        let dir = egui::vec2(angle.sin(), -angle.cos()) * size * 1.5;
        painter.line_segment(
            [pos, pos + dir],
            egui::Stroke::new(2.0, app.current_theme.text()),
        );
    }
}
