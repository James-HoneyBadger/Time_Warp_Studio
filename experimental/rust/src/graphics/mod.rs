use eframe::egui;
use image::{ImageBuffer, Rgba};
use imageproc::drawing::draw_antialiased_line_segment_mut;
// use imageproc::pixelops::interpolate;

/// A line segment drawn by the turtle
///
/// Represents a single draw operation with start/end points, color, and width.
/// Used for rendering and export to image formats.
#[derive(Debug, Clone)]
pub struct TurtleLine {
    pub start: egui::Pos2,
    pub end: egui::Pos2,
    pub color: egui::Color32,
    pub width: f32,
}

/// Turtle graphics state for Logo-style drawing
///
/// Maintains turtle position, heading, pen state, and drawing history.
/// Canvas coordinates: (0,0) is center, Y-axis inverted (up is negative).
///
/// # Example
/// ```ignore
/// let mut turtle = TurtleState::new();
/// turtle.forward(100.0);  // Move forward 100 pixels
/// turtle.right(90.0);     // Turn right 90 degrees
/// turtle.forward(100.0);  // Draw an L shape
/// ```
pub struct TurtleState {
    pub x: f32,
    pub y: f32,
    pub heading: f32, // degrees, 0 = up
    pub pen_down: bool,
    pub pen_color: egui::Color32,
    pub pen_width: f32,
    pub canvas_width: f32,
    pub canvas_height: f32,
    pub lines: Vec<TurtleLine>,
    pub visible: bool,
    pub bg_color: egui::Color32,
}

impl TurtleState {
    pub fn new() -> Self {
        Self {
            x: 0.0,
            y: 0.0,
            heading: 0.0,
            pen_down: true,
            pen_color: egui::Color32::WHITE,
            pen_width: 2.0,
            canvas_width: 800.0,
            canvas_height: 600.0,
            lines: Vec::new(),
            visible: true,
            bg_color: egui::Color32::from_rgb(10, 10, 20),
        }
    }

    pub fn forward(&mut self, distance: f32) {
        let rad = self.heading.to_radians();
        let old_x = self.x;
        let old_y = self.y;

        self.x += distance * rad.sin();
        self.y -= distance * rad.cos(); // Y is inverted in screen coordinates

        if self.pen_down {
            self.lines.push(TurtleLine {
                start: egui::pos2(old_x, old_y),
                end: egui::pos2(self.x, self.y),
                color: self.pen_color,
                width: self.pen_width,
            });
        }
    }

    pub fn back(&mut self, distance: f32) {
        self.forward(-distance);
    }

    pub fn left(&mut self, angle: f32) {
        self.heading -= angle;
        self.heading = self.heading.rem_euclid(360.0);
    }

    pub fn right(&mut self, angle: f32) {
        self.heading += angle;
        self.heading = self.heading.rem_euclid(360.0);
    }

    pub fn goto(&mut self, x: f32, y: f32) {
        if self.pen_down {
            self.lines.push(TurtleLine {
                start: egui::pos2(self.x, self.y),
                end: egui::pos2(x, y),
                color: self.pen_color,
                width: self.pen_width,
            });
        }
        self.x = x;
        self.y = y;
    }

    pub fn home(&mut self) {
        self.goto(0.0, 0.0);
        self.heading = 0.0;
    }

    pub fn clear(&mut self) {
        self.lines.clear();
    }

    #[allow(dead_code)]
    pub fn reset(&mut self) {
        self.x = 0.0;
        self.y = 0.0;
        self.heading = 0.0;
        self.pen_down = true;
        self.pen_color = egui::Color32::WHITE;
        self.pen_width = 2.0;
        self.lines.clear();
        self.visible = true;
        self.bg_color = egui::Color32::from_rgb(10, 10, 20);
    }

    /// Save canvas as PNG image
    pub fn save_png(&self, path: &str) -> anyhow::Result<()> {
        let width = self.canvas_width as u32;
        let height = self.canvas_height as u32;

        // Create image buffer
        let mut img: ImageBuffer<Rgba<u8>, Vec<u8>> = ImageBuffer::new(width, height);

        // Fill background
        for pixel in img.pixels_mut() {
            *pixel = Rgba([self.bg_color.r(), self.bg_color.g(), self.bg_color.b(), 255]);
        }

        // Draw lines (simple rasterization)
        for line in &self.lines {
            draw_line_aa_with_width(&mut img, line, width as f32, height as f32);
        }

        // Save to file
        img.save(path)?;
        Ok(())
    }
}

fn draw_line_aa_with_width(
    img: &mut ImageBuffer<Rgba<u8>, Vec<u8>>,
    line: &TurtleLine,
    canvas_w: f32,
    canvas_h: f32,
) {
    // Transform turtle coordinates (centered origin) to image coordinates (top-left origin)
    let cx = canvas_w / 2.0;
    let cy = canvas_h / 2.0;
    let x0 = (line.start.x + cx) as i32;
    let y0 = (cy - line.start.y) as i32;
    let x1 = (line.end.x + cx) as i32;
    let y1 = (cy - line.end.y) as i32;
    let base_color = Rgba([line.color.r(), line.color.g(), line.color.b(), 255]);
    // Compute normal for thickness approximation
    let dx = (x1 - x0) as f32;
    let dy = (y1 - y0) as f32;
    let len = (dx * dx + dy * dy).sqrt().max(1.0);
    let nx = -dy / len; // unit normal x
    let ny = dx / len; // unit normal y
    let strokes = line.width.max(1.0).round() as i32;
    let half = (strokes as f32 - 1.0) / 2.0;
    for i in 0..strokes {
        let offset = (i as f32 - half) * 0.9; // spacing factor
        let ox = (nx * offset).round() as i32;
        let oy = (ny * offset).round() as i32;
        draw_antialiased_line_segment_mut(
            img,
            (x0 + ox, y0 + oy),
            (x1 + ox, y1 + oy),
            base_color,
            |dst, color, c| {
                // Alpha blend with coverage c (0..1)
                let alpha = (c * (color[3] as f32 / 255.0)).clamp(0.0, 1.0);
                let inv = 1.0 - alpha;
                let nr = (color[0] as f32 * alpha + dst[0] as f32 * inv).round() as u8;
                let ng = (color[1] as f32 * alpha + dst[1] as f32 * inv).round() as u8;
                let nb = (color[2] as f32 * alpha + dst[2] as f32 * inv).round() as u8;
                Rgba([nr, ng, nb, 255])
            },
        );
    }
}

impl Default for TurtleState {
    fn default() -> Self {
        Self::new()
    }
}
