/*
 * canvas.h - GDI-based graphics canvas for turtle graphics
 * Supports zoom, pan, export, all turtle drawing commands
 */

#ifndef CANVAS_H
#define CANVAS_H

#include <windows.h>

/* Canvas functions */
HWND Canvas_Create(HWND hwndParent, HINSTANCE hInstance);
BOOL Canvas_IsCanvas(HWND hwnd);
void Canvas_Clear(HWND hwnd);
void Canvas_SetPenColor(HWND hwnd, COLORREF color);
void Canvas_SetPenWidth(HWND hwnd, int width);
void Canvas_MoveTo(HWND hwnd, int x, int y);
void Canvas_LineTo(HWND hwnd, int x, int y);
void Canvas_DrawCircle(HWND hwnd, int x, int y, int radius);
void Canvas_DrawRectangle(HWND hwnd, int x, int y, int width, int height);
void Canvas_DrawText(HWND hwnd, int x, int y, const TCHAR *text);
void Canvas_SetZoom(HWND hwnd, float zoom);
float Canvas_GetZoom(HWND hwnd);
void Canvas_Pan(HWND hwnd, int dx, int dy);
BOOL Canvas_Export(HWND hwnd, const TCHAR *filename);
void Canvas_ShowTurtle(HWND hwnd, BOOL show);
void Canvas_SetTurtlePos(HWND hwnd, int x, int y);
void Canvas_SetTurtleAngle(HWND hwnd, int angle);
void Canvas_GetTurtlePos(HWND hwnd, int *x, int *y);
int Canvas_GetTurtleAngle(HWND hwnd);

/* Compatibility wrappers expected by interpreters */
void Canvas_Forward(HWND hwnd, double distance);
void Canvas_Back(HWND hwnd, double distance);
void Canvas_Left(HWND hwnd, double angle);
void Canvas_Right(HWND hwnd, double angle);
void Canvas_PenUp(HWND hwnd);
void Canvas_PenDown(HWND hwnd);
void Canvas_Home(HWND hwnd);
void Canvas_SetXY(HWND hwnd, double x, double y);
void Canvas_Circle(HWND hwnd, int radius);
void Canvas_SetBgColor(HWND hwnd, COLORREF color);
void Canvas_DrawTurtle(HWND hwnd, int x, int y, int heading, BOOL penDown);

#endif /* CANVAS_H */
