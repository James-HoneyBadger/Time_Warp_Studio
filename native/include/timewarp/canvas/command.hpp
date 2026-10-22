// SPDX-License-Identifier: MIT
//
// Canvas commands: the vocabulary the interpreter uses to describe drawing.
//
// ---------------------------------------------------------------------------
// Why a flat, 72-byte, trivially copyable struct
// ---------------------------------------------------------------------------
//
// A command has to cross a thread boundary through a bounded ring buffer.  Both
// facts push in the same direction:
//
//   * Fixed size means a slot never has to be constructed or destroyed, so the
//     ring needs no placement new, no allocator, and no per-element bookkeeping.
//   * Trivially copyable means a record can be moved with `memcpy`, so the ring
//     can be a plain byte buffer and the hot path never calls a constructor.
//
// Payloads that are genuinely variable length -- a polygon's point list, or
// drawn text -- travel *after* the fixed header as trailing bytes, described by
// `point_bytes` / `text_bytes`.  That keeps the struct fixed-size without
// imposing an arbitrary limit on the length of either.
//
// Colour is packed into a single `uint32_t` rather than three channels.  That is
// not only 8 bytes smaller: it makes "did the colour change?" one comparison,
// which is exactly the question the queue's coalescing asks.

#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <type_traits>

namespace timewarp::canvas {

/// What a command asks the canvas to do.
///
/// The set mirrors `graphics/turtle_state.py`, which is the reference for what
/// the interpreters can actually emit.
enum class CanvasCommandKind : std::uint8_t {
    None = 0,

    // -- motion ----------------------------------------------------------
    Forward,   ///< x = distance
    Back,      ///< x = distance
    Left,      ///< x = degrees
    Right,     ///< x = degrees
    // There is deliberately no Pitch/Roll here. The third dimension is the
    // *host's* business: `TurtlePose` holds the orientation and projects to the
    // drawing plane before it speaks, so what the canvas receives is already
    // two-dimensional geometry. Two kinds that no host could ever emit were
    // declared for several releases and dropped by every consumer -- removing
    // them is the fix, not drawing them twice.
    Goto,      ///< x, y = position
    SetX,      ///< x = position
    SetY,      ///< y = position
    Home,      ///< no payload
    SetHeading, ///< x = degrees

    // -- pen and appearance ----------------------------------------------
    PenUp,             ///< no payload
    PenDown,           ///< no payload
    SetPenColor,       ///< color
    SetPenWidth,       ///< x = width
    SetPenStyle,       ///< index = style id, x = style parameter
    SetBackgroundColor, ///< color
    BeginFill,         ///< no payload
    EndFill,           ///< no payload
    FillLastShape,     ///< color
    ShowTurtle,        ///< no payload
    HideTurtle,        ///< no payload

    // -- canvas lifecycle -------------------------------------------------
    Clear,            ///< no payload
    Reset,            ///< no payload
    SetSvgaMode,      ///< x = width, y = height
    ClearSvgaMode,    ///< no payload
    /// A fixed-pixel screen: x = width, y = height. Drawing that follows is
    /// placed at these coordinates, y down from the top-left, and the surface
    /// stops fitting itself to the ink.
    SetFramebuffer,
    /// Back to the fit-to-content display list.
    ClearFramebuffer,
    SetPaletteColor,  ///< index = slot, color
    CyclePalette,     ///< index = steps

    // -- drawing ----------------------------------------------------------
    Circle,             ///< x = radius, y = extent
    DrawPoint,          ///< x, y = position, color
    DrawPolyline,       ///< trailing point list, color, x = width
    DrawRect,           ///< x, y, w, h, color
    DrawEllipse,        ///< x, y, w, h, color
    DrawArc,            ///< x, y, w, h, a = start, b = extent, color
    DrawText,           ///< x, y = position, w = size, trailing UTF-8 text
    DrawRectGradient,   ///< x, y, w, h, a = angle, color, b = second colour
    DrawEllipseGradient,///< x, y, w, h, color, b = second colour
    BezierCurve,        ///< trailing point list, x = width, color

    // -- sprites ----------------------------------------------------------
    /// A block of pixels: x, y = its top-left in raster coordinates, w, h its
    /// size, and the trailing doubles one packed `0x00RRGGBB` per pixel,
    /// row-major with the top row first. `PUT` emits it, so a captured sprite
    /// is visible and not merely remembered.
    PutPixels,
    /// trailing name; x, y = the hotspot (pivot) in pixels from the sprite's
    /// top-left, w, h its size, and the trailing doubles one packed
    /// `0x00RRGGBB` per pixel, row-major with the top row first.
    DefineSprite,
    PlaceSprite,   ///< trailing name; x, y = where the hotspot goes
    MoveSprite,    ///< trailing name; x, y = delta, applied to the last placement
    ShowSprite,    ///< trailing name
    HideSprite,    ///< trailing name
};

/// Per-command option bits.
enum CanvasCommandFlags : std::uint8_t {
    kCanvasFlagNone = 0,
    /// A `DrawPolyline` whose first and last points are joined.
    kCanvasFlagClosed = 1u << 0,
    /// A shape primitive that should be filled as well as outlined.
    kCanvasFlagFilled = 1u << 1,
    /// The pen is down, i.e. this motion leaves a trail.
    kCanvasFlagPenDown = 1u << 2,
};

/// Packs an 8-bit-per-channel RGB triple into the canonical `0x00RRGGBB` form.
///
/// Channels are masked rather than validated: a caller passing 256 gets the same
/// byte as one passing 0, which matches how Qt and the reference both treat an
/// out-of-range channel.
[[nodiscard]] constexpr std::uint32_t make_rgb(int red, int green,
                                               int blue) noexcept {
    return (static_cast<std::uint32_t>(red) & 0xFFu) << 16 |
           (static_cast<std::uint32_t>(green) & 0xFFu) << 8 |
           (static_cast<std::uint32_t>(blue) & 0xFFu);
}

/// One canvas operation.
///
/// Fields are named for their general role rather than any single kind, because
/// a fixed layout has to serve all of them; the per-kind meaning is documented
/// on `CanvasCommandKind`.  Every field defaults to zero, so a partially filled
/// command is well defined.
struct CanvasCommand {
    CanvasCommandKind kind = CanvasCommandKind::None;
    std::uint8_t flags = kCanvasFlagNone;
    std::uint16_t reserved = 0;
    /// UTF-8 bytes that follow the record.  A length, not a capacity.
    std::uint32_t text_bytes = 0;
    /// Bytes of `double` point data that follow the record, before the text.
    std::uint32_t point_bytes = 0;
    /// Palette slot, pen-style id, or step count, depending on `kind`.
    std::uint32_t index = 0;

    double x = 0.0;  ///< primary magnitude / position
    double y = 0.0;  ///< secondary magnitude / position
    double w = 0.0;  ///< width / size
    double h = 0.0;  ///< height
    double a = 0.0;  ///< angle / start
    double b = 0.0;  ///< extent / secondary angle / second colour

    /// Packed `0x00RRGGBB`.
    std::uint32_t color = 0;

    [[nodiscard]] constexpr int red() const noexcept {
        return static_cast<int>((color >> 16) & 0xFFu);
    }
    [[nodiscard]] constexpr int green() const noexcept {
        return static_cast<int>((color >> 8) & 0xFFu);
    }
    [[nodiscard]] constexpr int blue() const noexcept {
        return static_cast<int>(color & 0xFFu);
    }
    [[nodiscard]] constexpr bool is_closed() const noexcept {
        return (flags & kCanvasFlagClosed) != 0;
    }
    [[nodiscard]] constexpr bool is_filled() const noexcept {
        return (flags & kCanvasFlagFilled) != 0;
    }
    [[nodiscard]] constexpr bool pen_is_down() const noexcept {
        return (flags & kCanvasFlagPenDown) != 0;
    }
};

static_assert(std::is_trivially_copyable_v<CanvasCommand>,
              "a command must be memcpy-able to travel through the ring");
static_assert(sizeof(CanvasCommand) == 72,
              "the record header size is relied on by the queue");

/// A command together with its trailing variable-length payload.
///
/// `points` is interpreted as consecutive (x, y) pairs; the number of points is
/// `points.size() / 2`.  `text` is UTF-8 and is not necessarily terminated.
struct CanvasCommandView {
    const CanvasCommand& command;
    std::span<const double> points;
    std::string_view text;

    /// Number of (x, y) pairs in `points`.
    [[nodiscard]] std::size_t point_count() const noexcept {
        return points.size() / 2;
    }
};

/// Stable name, for diagnostics and tests.
[[nodiscard]] std::string_view to_string(CanvasCommandKind kind) noexcept;

/// True when a command is a pure "last value wins" state assignment.
///
/// Only these may be collapsed.  Anything that draws, moves the turtle, or
/// changes canvas structure must be delivered exactly once and in order --
/// collapsing `Forward` would silently delete a line segment.
///
/// Note the deliberate exclusion of `PenUp`/`PenDown` from coalescing *with each
/// other*: they are different kinds, and a command only collapses with an
/// identical predecessor.
[[nodiscard]] constexpr bool is_coalescible(CanvasCommandKind kind) noexcept {
    switch (kind) {
        case CanvasCommandKind::SetHeading:
        case CanvasCommandKind::SetX:
        case CanvasCommandKind::SetY:
        case CanvasCommandKind::SetPenColor:
        case CanvasCommandKind::SetPenWidth:
        case CanvasCommandKind::SetPenStyle:
        case CanvasCommandKind::SetBackgroundColor:
        case CanvasCommandKind::ShowTurtle:
        case CanvasCommandKind::HideTurtle:
        case CanvasCommandKind::SetPaletteColor:
            return true;
        default:
            return false;
    }
}

/// True when a command carries a trailing point list.
///
/// `DefineSprite` is here because a sprite is a *bitmap*: its payload is one
/// packed `0x00RRGGBB` per pixel, exactly like `PutPixels`, and it is counted in
/// doubles rather than in (x, y) pairs for the same reason. Making the sprite a
/// named region instead would have made a "define" cheap and a "place" a
/// re-render of the display list, which is neither what the reference's
/// `TurtleSprite` was nor what a sprite is for.
[[nodiscard]] constexpr bool has_points(CanvasCommandKind kind) noexcept {
    switch (kind) {
        case CanvasCommandKind::DrawPolyline:
        case CanvasCommandKind::BezierCurve:
        case CanvasCommandKind::DefineSprite:
            return true;
        default:
            return false;
    }
}

/// True when a command carries a trailing UTF-8 payload.
[[nodiscard]] constexpr bool has_text(CanvasCommandKind kind) noexcept {
    switch (kind) {
        case CanvasCommandKind::DrawText:
        case CanvasCommandKind::DefineSprite:
        case CanvasCommandKind::PlaceSprite:
        case CanvasCommandKind::MoveSprite:
        case CanvasCommandKind::ShowSprite:
        case CanvasCommandKind::HideSprite:
            return true;
        default:
            return false;
    }
}

// ---------------------------------------------------------------------------
// Factories
//
// Small, per-kind constructors so callers never assign to a bare field and
// wonder which slot a parameter belongs in.
// ---------------------------------------------------------------------------

[[nodiscard]] constexpr CanvasCommand make_command(
    CanvasCommandKind kind) noexcept {
    CanvasCommand command;
    command.kind = kind;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_motion(CanvasCommandKind kind,
                                                  double amount) noexcept {
    CanvasCommand command = make_command(kind);
    command.x = amount;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_position(double px,
                                                    double py) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::Goto);
    command.x = px;
    command.y = py;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_color(CanvasCommandKind kind,
                                                 std::uint32_t rgb) noexcept {
    CanvasCommand command = make_command(kind);
    command.color = rgb;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_draw_color(CanvasCommandKind kind,
                                                      int red, int green,
                                                      int blue) noexcept {
    return make_color(kind, make_rgb(red, green, blue));
}

[[nodiscard]] constexpr CanvasCommand make_pen_style(int style,
                                                     double parameter) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::SetPenStyle);
    command.index = static_cast<std::uint32_t>(style);
    command.x = parameter;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_palette_color(
    int slot, std::uint32_t rgb) noexcept {
    CanvasCommand command =
        make_color(CanvasCommandKind::SetPaletteColor, rgb);
    command.index = static_cast<std::uint32_t>(slot);
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_svga_mode(double width,
                                                     double height) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::SetSvgaMode);
    command.x = width;
    command.y = height;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_clear_svga_mode() noexcept {
    return make_command(CanvasCommandKind::ClearSvgaMode);
}

/// A palette slot's colour. `CyclePalette` then rotates every ink colour that
/// matches a slot through the slots, which is what palette cycling *is*.
[[nodiscard]] constexpr CanvasCommand make_cycle_palette(int steps) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::CyclePalette);
    command.index = static_cast<std::uint32_t>(steps < 0 ? -steps : steps);
    command.a = steps < 0 ? -1.0 : 1.0;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_fill_begin() noexcept {
    return make_command(CanvasCommandKind::BeginFill);
}

[[nodiscard]] constexpr CanvasCommand make_fill_end() noexcept {
    return make_command(CanvasCommandKind::EndFill);
}

/// Fills the shape drawn last with `rgb`. The polygon a `BeginFill`/`EndFill`
/// pair produced is the usual subject; a rect or an ellipse works too.
[[nodiscard]] constexpr CanvasCommand make_fill_last(std::uint32_t rgb) noexcept {
    return make_color(CanvasCommandKind::FillLastShape, rgb);
}

/// A gradient-filled rectangle. `angle` is degrees, measured like everything
/// else here: 0 runs left-to-right, 90 bottom-to-top.
[[nodiscard]] constexpr CanvasCommand make_rect_gradient(double x, double y,
                                                         double w, double h,
                                                         double angle,
                                                         std::uint32_t from,
                                                         std::uint32_t to) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::DrawRectGradient);
    command.x = x;
    command.y = y;
    command.w = w;
    command.h = h;
    command.a = angle;
    command.color = from;
    command.b = static_cast<double>(to);
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_ellipse_gradient(double x, double y,
                                                            double w, double h,
                                                            std::uint32_t from,
                                                            std::uint32_t to) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::DrawEllipseGradient);
    command.x = x;
    command.y = y;
    command.w = w;
    command.h = h;
    command.color = from;
    command.b = static_cast<double>(to);
    return command;
}

/// The second colour of a gradient, which rides in `b` because the record is a
/// fixed 72 bytes and `b` is the only slot a gradient does not otherwise use.
[[nodiscard]] constexpr std::uint32_t gradient_second(const CanvasCommand& command) noexcept {
    return static_cast<std::uint32_t>(command.b);
}

/// Placement of a defined sprite, with the hotspot at (x, y). The sprite's *name*
/// travels in the command's trailing text -- `queue.push(command, name)` -- the
/// same way a `DrawText`'s string does, because a name is variable-length and the
/// record is a fixed 72 bytes.
[[nodiscard]] constexpr CanvasCommand make_sprite_place(double x, double y) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::PlaceSprite);
    command.x = x;
    command.y = y;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_sprite_move(double dx, double dy) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::MoveSprite);
    command.x = dx;
    command.y = dy;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_sprite_visible(bool visible) noexcept {
    return make_command(visible ? CanvasCommandKind::ShowSprite
                                : CanvasCommandKind::HideSprite);
}

/// A fixed-pixel screen of `width x height`.
[[nodiscard]] constexpr CanvasCommand make_framebuffer(double width,
                                                       double height) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::SetFramebuffer);
    command.x = width;
    command.y = height;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_clear_framebuffer() noexcept {
    return make_command(CanvasCommandKind::ClearFramebuffer);
}

[[nodiscard]] constexpr CanvasCommand make_circle(double radius,
                                                  double extent = 360.0) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::Circle);
    command.x = radius;
    command.y = extent;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_draw_point(
    double px, double py, std::uint32_t rgb) noexcept {
    CanvasCommand command = make_color(CanvasCommandKind::DrawPoint, rgb);
    command.x = px;
    command.y = py;
    return command;
}

/// A pixel block: the packed colours follow as one double per pixel.
///
/// Unlike every other trailing payload, this one is **not** a point list: the
/// points span holds `width * height` doubles, each a packed colour, and a
/// reader must count `points.size()` rather than `point_count()`.
[[nodiscard]] constexpr CanvasCommand make_put_pixels(double px, double py,
                                                      double width,
                                                      double height) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::PutPixels);
    command.x = px;
    command.y = py;
    command.w = width;
    command.h = height;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_draw_rect(double px, double py,
                                                     double width,
                                                     double height) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::DrawRect);
    command.x = px;
    command.y = py;
    command.w = width;
    command.h = height;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_draw_ellipse(double px, double py,
                                                        double width,
                                                        double height) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::DrawEllipse);
    command.x = px;
    command.y = py;
    command.w = width;
    command.h = height;
    return command;
}

[[nodiscard]] constexpr CanvasCommand make_draw_arc(double px, double py,
                                                    double width, double height,
                                                    double start,
                                                    double extent) noexcept {
    CanvasCommand command = make_command(CanvasCommandKind::DrawArc);
    command.x = px;
    command.y = py;
    command.w = width;
    command.h = height;
    command.a = start;
    command.b = extent;
    return command;
}

/// Builds a polyline / bezier command; the point list is passed to `push()`.
[[nodiscard]] constexpr CanvasCommand make_polyline(
    CanvasCommandKind kind, std::uint32_t rgb, double width,
    bool closed = false, bool filled = false) noexcept {
    CanvasCommand command = make_color(kind, rgb);
    command.x = width;
    if (closed) {
        command.flags |= kCanvasFlagClosed;
    }
    if (filled) {
        command.flags |= kCanvasFlagFilled;
    }
    return command;
}

}  // namespace timewarp::canvas
