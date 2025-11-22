/*
 * Time Warp IDE - Haiku OS Edition
 * Turtle graphics canvas implementation
 */

#include "TurtleView.h"
#include <Bitmap.h>
#include <math.h>

TurtleView::TurtleView(const char *name)
    : BView(name, B_WILL_DRAW | B_FRAME_EVENTS),
      fOffscreen(NULL),
      fTurtlePos(400, 300),
      fTurtleAngle(0.0),
      fPenSize(1.0),
      fPenDown(true)
{
    fPenColor = (rgb_color){0, 0, 0, 255}; // Black
    SetViewColor(B_TRANSPARENT_COLOR);
}

TurtleView::~TurtleView()
{
    delete fOffscreen;
}

void TurtleView::AttachedToWindow()
{
    BView::AttachedToWindow();
    BRect bounds = Bounds();
    _InitBitmap(bounds.Width() + 1, bounds.Height() + 1);
}

void TurtleView::FrameResized(float width, float height)
{
    BView::FrameResized(width, height);
    _InitBitmap(width + 1, height + 1);
}

void TurtleView::_InitBitmap(float width, float height)
{
    delete fOffscreen;

    BRect bitmapBounds(0, 0, width - 1, height - 1);
    fOffscreen = new BBitmap(bitmapBounds, B_RGB32, true);

    if (fOffscreen->Lock())
    {
        BView *offscreenView = new BView(bitmapBounds, "offscreen",
                                         B_FOLLOW_NONE, B_WILL_DRAW);
        fOffscreen->AddChild(offscreenView);

        offscreenView->SetHighColor(255, 255, 255); // White background
        offscreenView->FillRect(bitmapBounds);

        fOffscreen->Unlock();
    }

    Invalidate();
}

void TurtleView::Draw(BRect updateRect)
{
    if (fOffscreen)
    {
        DrawBitmap(fOffscreen, updateRect, updateRect);
    }
}

void TurtleView::Clear()
{
    if (!fOffscreen || !fOffscreen->Lock())
        return;

    BView *offscreenView = fOffscreen->ChildAt(0);
    if (offscreenView)
    {
        offscreenView->SetHighColor(255, 255, 255);
        offscreenView->FillRect(fOffscreen->Bounds());
    }

    fOffscreen->Unlock();

    // Reset turtle to center
    BRect bounds = Bounds();
    fTurtlePos.Set(bounds.Width() / 2, bounds.Height() / 2);
    fTurtleAngle = 0.0;

    Invalidate();
}

void TurtleView::MoveTo(float x, float y)
{
    fTurtlePos.Set(x, y);
    Invalidate();
}

void TurtleView::LineTo(float x, float y)
{
    if (!fOffscreen || !fOffscreen->Lock())
        return;

    BView *offscreenView = fOffscreen->ChildAt(0);
    if (offscreenView && fPenDown)
    {
        offscreenView->SetHighColor(fPenColor);
        offscreenView->SetPenSize(fPenSize);
        offscreenView->StrokeLine(fTurtlePos, BPoint(x, y));
    }

    fOffscreen->Unlock();

    fTurtlePos.Set(x, y);
    Invalidate();
}

void TurtleView::SetPenColor(rgb_color color)
{
    fPenColor = color;
}

void TurtleView::SetPenSize(float size)
{
    fPenSize = size;
}

void TurtleView::_DrawTurtle()
{
    // Draw turtle indicator at current position
    // TODO: Implement turtle sprite
}
