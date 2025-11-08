/*
 * Time Warp IDE - Haiku OS Edition
 * Turtle graphics canvas using BBitmap
 */

#ifndef TURTLE_VIEW_H
#define TURTLE_VIEW_H

#include <View.h>
#include <Bitmap.h>
#include <Point.h>

class TurtleView : public BView
{
public:
    TurtleView(const char *name);
    virtual ~TurtleView();

    virtual void AttachedToWindow();
    virtual void Draw(BRect updateRect);
    virtual void FrameResized(float width, float height);

    void Clear();
    void MoveTo(float x, float y);
    void LineTo(float x, float y);
    void SetPenColor(rgb_color color);
    void SetPenSize(float size);

    BPoint GetTurtlePosition() const { return fTurtlePos; }
    float GetTurtleAngle() const { return fTurtleAngle; }
    void SetTurtleAngle(float angle) { fTurtleAngle = angle; }

private:
    void _InitBitmap(float width, float height);
    void _DrawTurtle();

    BBitmap *fOffscreen;
    BPoint fTurtlePos;
    float fTurtleAngle;
    rgb_color fPenColor;
    float fPenSize;
    bool fPenDown;
};

#endif // TURTLE_VIEW_H
