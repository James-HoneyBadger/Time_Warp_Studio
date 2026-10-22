// SPDX-License-Identifier: MIT
//
// The drawing surface a native language executor talks to.
//
// Deliberately abstract and Qt-free, for the same reason `scheme::Host` is: an
// executor has to be testable without a window, and the same program has to run
// headless, in the IDE and in a differential harness. The IDE adapter turns
// these calls into `CanvasCommand`s (see `lang/qt_host.hpp`); a test adapter
// records them as text.
//
// The methods are the union of the turtle extensions the line-by-line
// languages expose (FOCAL, COMAL, REXX, Logo) plus the raster operations CHIP-8
// needs. Adding a language extends this interface rather than reaching around
// it, so every executor draws through the same door.

#pragma once

#include <array>
#include <cmath>
#include <cstdint>
#include <string>
#include <vector>

namespace timewarp::lang {

/// An 8-bit-per-channel colour. The engine never names a palette index in a
/// draw call: `resolve_color` is asked once and the result travels with the
/// command, so the picture does not depend on when the GUI applies a palette.
struct Rgb {
    int red = 0;
    int green = 0;
    int blue = 0;

    [[nodiscard]] friend constexpr bool operator==(const Rgb&, const Rgb&) = default;
};

/// The sixteen colours the reference's palette starts with.
///
/// A program that names a palette slot -- `SETPC 1`, `COLOR 1`, `1 SETPC` --
/// asks for the slot's colour, and the reference seeded those sixteen before any
/// program ran. Two hosts that each invented their own answer made the same
/// statement draw in different colours in different languages, so the table is
/// written once, here, and every host asks it.
///
/// Indices past 15 are the grey of their own number, which is what the reference
/// did for an unset slot in 0..255. Anything outside 0..255 is not a slot, and
/// this returns false so the caller can supply its own fallback.
[[nodiscard]] inline bool reference_palette_colour(int index, Rgb* out) {
    static constexpr std::array<Rgb, 16> kSlots{{
        Rgb{0, 0, 0},       Rgb{0, 0, 170},     Rgb{0, 170, 0},   Rgb{0, 170, 170},
        Rgb{170, 0, 0},     Rgb{170, 0, 170},   Rgb{170, 85, 0},  Rgb{170, 170, 170},
        Rgb{85, 85, 85},    Rgb{85, 85, 255},   Rgb{85, 255, 85}, Rgb{85, 255, 255},
        Rgb{255, 85, 85},   Rgb{255, 85, 255},  Rgb{255, 255, 85}, Rgb{255, 255, 255},
    }};
    if (index < 0 || index > 255) {
        return false;
    }
    if (out != nullptr) {
        *out = index < static_cast<int>(kSlots.size())
                   ? kSlots[static_cast<std::size_t>(index)]
                   : Rgb{index, index, index};
    }
    return true;
}

/// A point on the drawing surface.
struct Point {
    double x = 0.0;
    double y = 0.0;
};

/// The turtle's pose, with the reference's arithmetic.
///
/// Heading 0 is **north** and increases clockwise, so one step of `distance`
/// is `x += d*sin(h), y += d*cos(h)`. That is unusual enough to have been
/// mistaken for a bug, and it is asserted in `turtle_canvas_tests`; keeping the
/// maths here means a host and an executor that both need the pose cannot
/// disagree about it.
///
/// The 3-D members are the reference's: a heading vector `hv`, a left vector
/// `lv` and an up vector `uv`, rotated by Rodrigues' formula and projected to
/// the canvas by `project`. Only Logo's `ENABLE3D`/`PITCH`/`ROLL` reach them,
/// and `Examples/logo/turtle_3d.logo` is drawn by them, so they are part of the
/// surface rather than one language's private business.
class TurtlePose {
public:
    static constexpr double kPerspective = 400.0;

    [[nodiscard]] double x() const noexcept { return x_; }
    [[nodiscard]] double y() const noexcept { return y_; }
    [[nodiscard]] double z() const noexcept { return z_; }
    [[nodiscard]] double heading() const noexcept { return heading_; }
    [[nodiscard]] bool is_3d() const noexcept { return is_3d_; }

    /// Walks `distance` along the current heading, in 2-D or in 3-D.
    void move(double distance) noexcept;
    /// Turns clockwise for `right`, anti-clockwise for `left`.
    void turn(double degrees) noexcept;
    void set_heading(double degrees) noexcept;
    void go_to(double px, double py) noexcept;
    void home() noexcept;

    void enable_3d() noexcept;
    void disable_3d() noexcept;
    void pitch(double degrees) noexcept;
    void roll(double degrees) noexcept;

    /// The canvas point a 3-D position projects to. A 2-D pose is returned as
    /// it stands, which is what the reference does (`z` is zero).
    [[nodiscard]] Point project() const noexcept;

private:
    static double wrap(double degrees) noexcept;
    static std::array<double, 3> rotate(const std::array<double, 3>& vector,
                                        const std::array<double, 3>& axis,
                                        double degrees) noexcept;

    double x_ = 0.0;
    double y_ = 0.0;
    double z_ = 0.0;
    double heading_ = 0.0;
    bool is_3d_ = false;
    std::array<double, 3> hv_{0.0, 1.0, 0.0};
    std::array<double, 3> lv_{-1.0, 0.0, 0.0};
    std::array<double, 3> uv_{0.0, 0.0, 1.0};
};

inline double TurtlePose::wrap(double degrees) noexcept {
    // Python's `%` on a negative value returns a non-negative remainder, and
    // HEADING reports what it returns.
    const double wrapped = std::fmod(degrees, 360.0);
    return wrapped < 0.0 ? wrapped + 360.0 : wrapped;
}

inline std::array<double, 3> TurtlePose::rotate(const std::array<double, 3>& vector,
                                                const std::array<double, 3>& axis,
                                                double degrees) noexcept {
    constexpr double kPi = 3.14159265358979323846;
    const double angle = degrees * kPi / 180.0;
    const double c = std::cos(angle);
    const double s = std::sin(angle);
    const double dot = axis[0] * vector[0] + axis[1] * vector[1] + axis[2] * vector[2];
    const double cx = axis[1] * vector[2] - axis[2] * vector[1];
    const double cy = axis[2] * vector[0] - axis[0] * vector[2];
    const double cz = axis[0] * vector[1] - axis[1] * vector[0];
    return {vector[0] * c + cx * s + axis[0] * dot * (1.0 - c),
            vector[1] * c + cy * s + axis[1] * dot * (1.0 - c),
            vector[2] * c + cz * s + axis[2] * dot * (1.0 - c)};
}

inline void TurtlePose::move(double distance) noexcept {
    if (is_3d_) {
        x_ += hv_[0] * distance;
        y_ += hv_[1] * distance;
        z_ += hv_[2] * distance;
        return;
    }
    constexpr double kPi = 3.14159265358979323846;
    const double radians = heading_ * kPi / 180.0;
    x_ += distance * std::sin(radians);
    y_ += distance * std::cos(radians);
}

inline void TurtlePose::turn(double degrees) noexcept {
    if (is_3d_) {
        // Turning right is a *negative* rotation about the up vector in the
        // reference, so `turn` takes the heading's sense and rotates the other
        // way.
        hv_ = rotate(hv_, uv_, -degrees);
        lv_ = rotate(lv_, uv_, -degrees);
    }
    heading_ = wrap(heading_ + degrees);
}

inline void TurtlePose::set_heading(double degrees) noexcept { heading_ = wrap(degrees); }

inline void TurtlePose::go_to(double px, double py) noexcept {
    x_ = px;
    y_ = py;
}

inline void TurtlePose::home() noexcept {
    x_ = 0.0;
    y_ = 0.0;
    z_ = 0.0;
    heading_ = 0.0;
    hv_ = {0.0, 1.0, 0.0};
    lv_ = {-1.0, 0.0, 0.0};
    uv_ = {0.0, 0.0, 1.0};
}

inline void TurtlePose::enable_3d() noexcept {
    is_3d_ = true;
    z_ = 0.0;
    hv_ = {0.0, 1.0, 0.0};
    lv_ = {-1.0, 0.0, 0.0};
    uv_ = {0.0, 0.0, 1.0};
}

inline void TurtlePose::disable_3d() noexcept {
    is_3d_ = false;
    z_ = 0.0;
}

inline void TurtlePose::pitch(double degrees) noexcept {
    if (!is_3d_) {
        return;
    }
    const std::array<double, 3> right{-lv_[0], -lv_[1], -lv_[2]};
    hv_ = rotate(hv_, right, degrees);
    uv_ = rotate(uv_, right, degrees);
}

inline void TurtlePose::roll(double degrees) noexcept {
    if (!is_3d_) {
        return;
    }
    lv_ = rotate(lv_, hv_, degrees);
    uv_ = rotate(uv_, hv_, degrees);
}

inline Point TurtlePose::project() const noexcept {
    double denominator = 1.0 + z_ / kPerspective;
    if (std::fabs(denominator) < 1e-6) {
        denominator = 1e-6;
    }
    return Point{x_ / denominator, y_ / denominator};
}

class TurtleHost {
public:
    virtual ~TurtleHost() = default;

    // -- turtle motion ----------------------------------------------------
    virtual void forward(double distance) = 0;
    virtual void back(double distance) = 0;
    virtual void right(double degrees) = 0;
    virtual void left(double degrees) = 0;
    virtual void goto_xy(double x, double y) = 0;
    virtual void set_heading(double degrees) = 0;
    virtual void home() = 0;
    /// Wipes the drawing and leaves the turtle where it is, as `CLEAN` does.
    virtual void clear() = 0;
    /// Everything back to how a run starts: position, heading, pen and a blank
    /// canvas. Logo's `CS` is this and not `clear`, which is the difference the
    /// reference draws between `CLEARSCREEN` and `CLEAN`.
    virtual void reset() = 0;

    // -- 3-D pose ---------------------------------------------------------
    // Logo's `ENABLE3D`/`PITCH`/`ROLL`. Only the adapter that renders needs to
    // do anything with them; a recording host notes the call and moves on.
    virtual void enable_3d() = 0;
    virtual void disable_3d() = 0;
    virtual void pitch(double degrees) = 0;
    virtual void roll(double degrees) = 0;

    // -- pen --------------------------------------------------------------
    virtual void pen_up() = 0;
    virtual void pen_down() = 0;
    virtual void set_pen_width(double width) = 0;
    virtual void set_color(Rgb colour) = 0;
    virtual void set_background(Rgb colour) = 0;

    // -- shapes -----------------------------------------------------------
    virtual void circle(double radius, double extent = 360.0) = 0;
    /// A filled rectangle in world coordinates; y is up, as everywhere else.
    virtual void draw_rect(double x0, double y0, double x1, double y1, Rgb fill) = 0;
    /// Text at a world position. `font_size` is the height in pixels, the same
    /// number `TurtleState.draw_text` takes.
    virtual void draw_text(double x, double y, const char* text, int font_size = 12) = 0;
    virtual void draw_point(double x, double y, Rgb colour) = 0;
    /// Fills the shape most recently drawn, as PILOT's `G: FILL` does.
    virtual void fill_last_shape(Rgb fill) = 0;
    /// A polygon drawn without moving the turtle, as Logo's `STAMP`, `DOT` and
    /// `FILLED` need. `fill` applies only when `filled` is true.
    virtual void draw_polygon(const std::vector<Point>& points, Rgb stroke, Rgb fill,
                              double width, bool filled) = 0;

    /// A block of pixels placed at a point, row-major, top row first.
    ///
    /// The one raster operation that has to reach the surface: `PUT` restores a
    /// sprite the program captured, and a host that cannot show it would make
    /// `GET`/`PUT` an invisible feature. Not pure, because the hosts that draw
    /// no raster -- a test recorder, a headless gate -- lose nothing by
    /// ignoring it, and a default keeps every existing host compiling.
    virtual void put_pixels(int /*x*/, int /*y*/, int /*width*/, int /*height*/,
                            const std::vector<Rgb>& /*pixels*/) {}

    // -- introspection ------------------------------------------------------
    // The reference's executors can read `turtle.pen_color` and use it as the
    // fill colour. One value is cheap to mirror and is exactly what a command
    // has already told the surface; the full pose is not mirrored here, because
    // the GUI owns it and a second copy would be the one that drifts. An
    // executor that needs the pose (Logo's `POS`, `STAMP` and `FILLED` do)
    // keeps a `TurtlePose` of its own, built from the same arithmetic.
    [[nodiscard]] virtual Rgb pen_colour() const = 0;

    // -- display ----------------------------------------------------------
    virtual void show_turtle() = 0;
    virtual void hide_turtle() = 0;
    virtual void set_svga_mode(int width, int height) = 0;
    /// A program asked for a fixed-pixel screen: `width x height` pixels at
    /// fixed coordinates, y down, origin top-left. `SCREEN` is the statement
    /// that says it.
    ///
    /// Distinct from `set_svga_mode`, which reports the *world-space* size a
    /// whole-program engine happens to draw in (CHIP-8 fills a 512x256 world
    /// rectangle) and does not by itself change how anything is shown. Not
    /// pure: a recorder or headless gate has no surface, and the program still
    /// runs when the request is ignored.
    virtual void set_framebuffer(int /*width*/, int /*height*/) {}
    /// Back to the display list: `SCREEN 0`, or a program that left graphics
    /// mode. A host that never had a framebuffer does nothing.
    virtual void clear_framebuffer() {}
    virtual void set_palette_color(int index, Rgb colour) = 0;
    /// Rotates the palette `[from, to]` by `step` slots.
    ///
    /// A default no-op rather than pure, because a host that has no palette (a
    /// recording host, a test double) is not wrong to ignore it -- and forcing
    /// every host to implement animation would have made this cost more than it
    /// returns. `NativeCanvasHost` overrides it, which is the host a user sees.
    virtual void cycle_palette(int /*from*/, int /*to*/, int /*step*/) {}
    /// Sets the pen's dash style: 0 solid, 1 dashed, 2 dotted, 3 dash-dot,
    /// 4 invisible. Same reasoning as `cycle_palette` for the default.
    virtual void set_pen_style(int /*style*/, double /*parameter*/) {}

    // -- shapes a program can ask for ------------------------------------
    //
    // Everything below was drawable by the canvas for a release before a
    // language could reach it, which is the failure this interface exists to
    // stop repeating. Same default-no-op reasoning as `cycle_palette`: a host
    // with no bitmap surface is not wrong to ignore a sprite.

    /// A cubic bezier: `points[0]` is where it starts, then each group of three
    /// is two control points and an end point. Four points is one segment.
    virtual void bezier(const std::vector<Point>& /*points*/, Rgb /*stroke*/,
                        double /*width*/) {}
    /// A rectangle filled with a gradient from `from` to `to`, shaded along
    /// `angle` degrees (0 runs left to right). The corners may arrive in either
    /// order.
    virtual void draw_rect_gradient(double /*x0*/, double /*y0*/, double /*x1*/,
                                    double /*y1*/, double /*angle*/, Rgb /*from*/,
                                    Rgb /*to*/) {}
    /// The same for an ellipse inscribed in the two corners.
    virtual void draw_ellipse_gradient(double /*x0*/, double /*y0*/, double /*x1*/,
                                       double /*y1*/, Rgb /*from*/, Rgb /*to*/) {}

    /// A named bitmap: `pixels` is row-major from the top row, one per pixel,
    /// and `hotspot` is the point within it that `place_sprite` positions. The
    /// canvas keeps the name, so a placed sprite can move without the engine
    /// resending its pixels.
    virtual void define_sprite(const char* /*name*/, int /*hotspot_x*/, int /*hotspot_y*/,
                               int /*width*/, int /*height*/,
                               const std::vector<Rgb>& /*pixels*/) {}
    /// Draws a defined sprite's bitmap with its hotspot at (x, y). A name that
    /// was never defined draws nothing; the engine reports that, because it is
    /// the part that knows the program.
    virtual void place_sprite(const char* /*name*/, double /*x*/, double /*y*/) {}
    /// Moves the newest placement of a sprite by a delta.
    virtual void move_sprite(const char* /*name*/, double /*dx*/, double /*dy*/) {}
    /// Shows or hides every placement of a sprite.
    virtual void set_sprite_visible(const char* /*name*/, bool /*visible*/) {}
    /// The colour a palette index currently means. Used where a program draws a
    /// whole frame and must name the colour, not the index.
    [[nodiscard]] virtual Rgb resolve_color(int index) const = 0;

    // -- sound ------------------------------------------------------------
    //
    // Not pure: a host with no audio (a test recorder, a headless gate) ignores
    // these, and the language still runs. `audio_available` is what an executor
    // asks before promising a sound, so the answer a learner is given matches the
    // machine they are on.

    /// The system bell: `BEEP`. Short, and whatever the system uses for a bell.
    virtual void beep(int /*milliseconds*/) {}
    /// One tone: `SOUND frequency, duration`. A frequency of 0 is a rest, and
    /// `waveform` is "square", "triangle", "sine" or "noise".
    virtual void tone(double /*frequency*/, int /*milliseconds*/, const char* /*waveform*/) {}
    /// A note sequence in `PLAY`'s mini-language, played in the background.
    virtual void play_music(const char* /*notation*/) {}
    /// Silence. `SOUND OFF`.
    virtual void stop_sound() {}
    [[nodiscard]] virtual bool audio_available() const { return false; }

    // -- keyboard ---------------------------------------------------------

    /// The characters typed at the drawing surface since the last call, oldest
    /// first, and clears them. A headless run has none, which is why `INKEY$`'s
    /// documented answer is the empty string there.
    [[nodiscard]] virtual std::string take_keystrokes() { return {}; }
};

}  // namespace timewarp::lang
