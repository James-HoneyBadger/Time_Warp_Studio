// SPDX-License-Identifier: MIT

#include "timewarp/canvas/command.hpp"

namespace timewarp::canvas {

std::string_view to_string(CanvasCommandKind kind) noexcept {
    switch (kind) {
        case CanvasCommandKind::None:
            return "None";

        case CanvasCommandKind::Forward:
            return "Forward";
        case CanvasCommandKind::Back:
            return "Back";
        case CanvasCommandKind::Left:
            return "Left";
        case CanvasCommandKind::Right:
            return "Right";
        case CanvasCommandKind::Goto:
            return "Goto";
        case CanvasCommandKind::SetX:
            return "SetX";
        case CanvasCommandKind::SetY:
            return "SetY";
        case CanvasCommandKind::Home:
            return "Home";
        case CanvasCommandKind::SetHeading:
            return "SetHeading";

        case CanvasCommandKind::PenUp:
            return "PenUp";
        case CanvasCommandKind::PenDown:
            return "PenDown";
        case CanvasCommandKind::SetPenColor:
            return "SetPenColor";
        case CanvasCommandKind::SetPenWidth:
            return "SetPenWidth";
        case CanvasCommandKind::SetPenStyle:
            return "SetPenStyle";
        case CanvasCommandKind::SetBackgroundColor:
            return "SetBackgroundColor";
        case CanvasCommandKind::BeginFill:
            return "BeginFill";
        case CanvasCommandKind::EndFill:
            return "EndFill";
        case CanvasCommandKind::FillLastShape:
            return "FillLastShape";
        case CanvasCommandKind::ShowTurtle:
            return "ShowTurtle";
        case CanvasCommandKind::HideTurtle:
            return "HideTurtle";

        case CanvasCommandKind::Clear:
            return "Clear";
        case CanvasCommandKind::Reset:
            return "Reset";
        case CanvasCommandKind::SetSvgaMode:
            return "SetSvgaMode";
        case CanvasCommandKind::SetFramebuffer:
            return "SetFramebuffer";
        case CanvasCommandKind::ClearFramebuffer:
            return "ClearFramebuffer";
        case CanvasCommandKind::SetPaletteColor:
            return "SetPaletteColor";
        case CanvasCommandKind::CyclePalette:
            return "CyclePalette";

        case CanvasCommandKind::Circle:
            return "Circle";
        case CanvasCommandKind::DrawPoint:
            return "DrawPoint";
        case CanvasCommandKind::DrawPolyline:
            return "DrawPolyline";
        case CanvasCommandKind::DrawRect:
            return "DrawRect";
        case CanvasCommandKind::DrawEllipse:
            return "DrawEllipse";
        case CanvasCommandKind::DrawArc:
            return "DrawArc";
        case CanvasCommandKind::DrawText:
            return "DrawText";
        case CanvasCommandKind::DrawRectGradient:
            return "DrawRectGradient";
        case CanvasCommandKind::DrawEllipseGradient:
            return "DrawEllipseGradient";
        case CanvasCommandKind::BezierCurve:
            return "BezierCurve";

        case CanvasCommandKind::PutPixels:
            return "PutPixels";
        case CanvasCommandKind::DefineSprite:
            return "DefineSprite";
        case CanvasCommandKind::PlaceSprite:
            return "PlaceSprite";
        case CanvasCommandKind::MoveSprite:
            return "MoveSprite";
        case CanvasCommandKind::ShowSprite:
            return "ShowSprite";
        case CanvasCommandKind::HideSprite:
            return "HideSprite";
    }
    return "Unknown";
}

}  // namespace timewarp::canvas
