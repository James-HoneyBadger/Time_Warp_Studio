// SPDX-License-Identifier: MIT
//
// The interface's icons, drawn rather than borrowed.
//
// Every control used to carry an emoji in its label: a hammer for New, a floppy for
// Save, a rocket for Run. Emoji are rendered by whichever font supplies them, at
// whichever weight that font chose, in colour that ignores the theme — so a toolbar of
// them reads as a row of unrelated pictures, and a selection of them reads as a row of
// unrelated pictures in different sizes.
//
// These are geometric glyphs on a 16-unit grid: 1.5-unit strokes, round caps, one
// colour, no fills except where a shape has to read at 16 pixels (Run, Stop, Pause).
// The tint comes from the palette at paint time through an icon engine, so a theme
// change repaints every icon without anything having to be rebuilt.

#pragma once

#include <QColor>
#include <QIcon>
#include <QPalette>

namespace timewarp::ui::icons {

/// Every glyph the interface uses. `None` answers an empty icon, which is what a
/// control that is only a label wants.
enum class Glyph {
    None,
    New,
    Open,
    Save,
    Run,
    Pause,
    Stop,
    Continue,
    Step,
    Trace,
    Debug,
    Snippets,
    ClearOutput,
    ClearCanvas,
    TextScale,
    Settings,
    Console,
    Graphics,
    Variables,
    Telemetry,
    Lessons,
    Classroom,
    Projects,
    Terminal,
    Database,
    Reference,
    Notebook,
    Gallery,
    Folder,
    File,
    Close,
};

/// An icon that takes its colour from the widget palette when it is painted.
///
/// `role` is the palette entry to tint with: `WindowText` for a toolbar or a dock
/// title, `ButtonText` inside a menu, `HighlightedText` for a selected tab. The
/// `Selected` and `Disabled` states are answered with the matching palette entry and
/// a dimmed tint, so one icon serves every state a control can be in.
[[nodiscard]] QIcon glyph(Glyph id, QPalette::ColorRole role = QPalette::WindowText);

/// The same glyph in one fixed colour, for a surface that is painted rather than
/// styled — the splash card, a rendered report, a test.
[[nodiscard]] QIcon glyph(Glyph id, const QColor& tint);

/// Paints `id` into `painter` at `size` in `tint`, on a transparent background. The
/// window's own painting uses this directly; `glyph()` is the same drawing wrapped in
/// an icon engine.
void paint(QPainter& painter, Glyph id, const QRectF& bounds, const QColor& tint);

}  // namespace timewarp::ui::icons
