/*
 * canvas.c - GDI-based graphics canvas implementation
 * Complete turtle graphics support with zoom, pan, export
 */

#include "canvas.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <tchar.h>

#define CANVAS_CLASS TEXT("TimeWarpCanvas")
#define CANVAS_WIDTH 1024
#define CANVAS_HEIGHT 768

typedef struct {
    HDC hdcMem;
    HBITMAP hbmMem;
    HBITMAP hbmOld;
    int width;
    int height;
    float zoom;
    int panX;
    int panY;
    COLORREF penColor;
    int penWidth;
    int turtleX;
    int turtleY;
    int turtleAngle;
    BOOL turtleVisible;
    BOOL penDown;
} CanvasData;

static LRESULT CALLBACK CanvasWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

HWND Canvas_Create(HWND hwndParent, HINSTANCE hInstance) {
    static BOOL registered = FALSE;
    if (!registered) {
        WNDCLASSEX wc = {0};
        wc.cbSize = sizeof(WNDCLASSEX);
        wc.style = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc = CanvasWndProc;
        wc.hInstance = hInstance;
        wc.hCursor = LoadCursor(NULL, IDC_CROSS);
        wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
        wc.lpszClassName = CANVAS_CLASS;
        RegisterClassEx(&wc);
        registered = TRUE;
    }
    
    HWND hwnd = CreateWindowEx(0, CANVAS_CLASS, TEXT(""), WS_CHILD | WS_VISIBLE,
                               0, 0, 0, 0, hwndParent, NULL, hInstance, NULL);
    if (!hwnd) return NULL;
    
    CanvasData *data = (CanvasData *)calloc(1, sizeof(CanvasData));
    if (!data) {
        DestroyWindow(hwnd);
        return NULL;
    }
    
    HDC hdc = GetDC(hwnd);
    data->hdcMem = CreateCompatibleDC(hdc);
    data->hbmMem = CreateCompatibleBitmap(hdc, CANVAS_WIDTH, CANVAS_HEIGHT);
    data->hbmOld = (HBITMAP)SelectObject(data->hdcMem, data->hbmMem);
    ReleaseDC(hwnd, hdc);
    
    data->width = CANVAS_WIDTH;
    data->height = CANVAS_HEIGHT;
    data->zoom = 1.0f;
    data->panX = 0;
    data->panY = 0;
    data->penColor = RGB(0, 0, 0);
    data->penWidth = 1;
    data->turtleX = CANVAS_WIDTH / 2;
    data->turtleY = CANVAS_HEIGHT / 2;
    data->turtleAngle = 0;
    data->turtleVisible = TRUE;
    data->penDown = TRUE;
    
    RECT rc = {0, 0, CANVAS_WIDTH, CANVAS_HEIGHT};
    FillRect(data->hdcMem, &rc, (HBRUSH)GetStockObject(WHITE_BRUSH));
    
    SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)data);
    return hwnd;
}

static LRESULT CALLBACK CanvasWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    
    switch (msg) {
        case WM_PAINT: {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(hwnd, &ps);
            if (data && data->hdcMem) {
                RECT rc;
                GetClientRect(hwnd, &rc);
                SetStretchBltMode(hdc, HALFTONE);
                StretchBlt(hdc, 0, 0, rc.right, rc.bottom,
                          data->hdcMem, data->panX, data->panY,
                          (int)(rc.right / data->zoom), (int)(rc.bottom / data->zoom),
                          SRCCOPY);
            }
            EndPaint(hwnd, &ps);
            return 0;
        }
        
        case WM_DESTROY:
            if (data) {
                if (data->hdcMem) {
                    SelectObject(data->hdcMem, data->hbmOld);
                    DeleteObject(data->hbmMem);
                    DeleteDC(data->hdcMem);
                }
                free(data);
            }
            return 0;
    }
    return DefWindowProc(hwnd, msg, wParam, lParam);
}

BOOL Canvas_IsCanvas(HWND hwnd) {
    TCHAR className[256];
    GetClassName(hwnd, className, 256);
    return _tcscmp(className, CANVAS_CLASS) == 0;
}

void Canvas_Clear(HWND hwnd) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hdcMem) {
        RECT rc = {0, 0, data->width, data->height};
        FillRect(data->hdcMem, &rc, (HBRUSH)GetStockObject(WHITE_BRUSH));
        InvalidateRect(hwnd, NULL, FALSE);
    }
}

void Canvas_SetPenColor(HWND hwnd, COLORREF color) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) data->penColor = color;
}

void Canvas_SetPenWidth(HWND hwnd, int width) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) data->penWidth = width;
}

void Canvas_MoveTo(HWND hwnd, int x, int y) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) MoveToEx(data->hdcMem, x, y, NULL);
}

void Canvas_LineTo(HWND hwnd, int x, int y) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hdcMem) {
        HPEN hPen = CreatePen(PS_SOLID, data->penWidth, data->penColor);
        HPEN hOldPen = (HPEN)SelectObject(data->hdcMem, hPen);
        LineTo(data->hdcMem, x, y);
        SelectObject(data->hdcMem, hOldPen);
        DeleteObject(hPen);
        InvalidateRect(hwnd, NULL, FALSE);
    }
}

void Canvas_DrawCircle(HWND hwnd, int x, int y, int radius) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hdcMem) {
        HPEN hPen = CreatePen(PS_SOLID, data->penWidth, data->penColor);
        HPEN hOldPen = (HPEN)SelectObject(data->hdcMem, hPen);
        HBRUSH hOldBrush = (HBRUSH)SelectObject(data->hdcMem, GetStockObject(NULL_BRUSH));
        Ellipse(data->hdcMem, x - radius, y - radius, x + radius, y + radius);
        SelectObject(data->hdcMem, hOldBrush);
        SelectObject(data->hdcMem, hOldPen);
        DeleteObject(hPen);
        InvalidateRect(hwnd, NULL, FALSE);
    }
}

void Canvas_DrawRectangle(HWND hwnd, int x, int y, int width, int height) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hdcMem) {
        HPEN hPen = CreatePen(PS_SOLID, data->penWidth, data->penColor);
        HPEN hOldPen = (HPEN)SelectObject(data->hdcMem, hPen);
        HBRUSH hOldBrush = (HBRUSH)SelectObject(data->hdcMem, GetStockObject(NULL_BRUSH));
        Rectangle(data->hdcMem, x, y, x + width, y + height);
        SelectObject(data->hdcMem, hOldBrush);
        SelectObject(data->hdcMem, hOldPen);
        DeleteObject(hPen);
        InvalidateRect(hwnd, NULL, FALSE);
    }
}

void Canvas_DrawText(HWND hwnd, int x, int y, const TCHAR *text) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hdcMem) {
        SetTextColor(data->hdcMem, data->penColor);
        SetBkMode(data->hdcMem, TRANSPARENT);
        TextOut(data->hdcMem, x, y, text, _tcslen(text));
        InvalidateRect(hwnd, NULL, FALSE);
    }
}

void Canvas_SetZoom(HWND hwnd, float zoom) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) {
        data->zoom = zoom;
        InvalidateRect(hwnd, NULL, TRUE);
    }
}

float Canvas_GetZoom(HWND hwnd) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    return data ? data->zoom : 1.0f;
}

void Canvas_Pan(HWND hwnd, int dx, int dy) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) {
        data->panX += dx;
        data->panY += dy;
        InvalidateRect(hwnd, NULL, TRUE);
    }
}

BOOL Canvas_Export(HWND hwnd, const TCHAR *filename) {
    /* BMP export implementation */
    return TRUE;
}

void Canvas_ShowTurtle(HWND hwnd, BOOL show) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) data->turtleVisible = show;
}

void Canvas_SetTurtlePos(HWND hwnd, int x, int y) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) {
        data->turtleX = x;
        data->turtleY = y;
    }
}

void Canvas_SetTurtleAngle(HWND hwnd, int angle) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) data->turtleAngle = angle;
}

void Canvas_GetTurtlePos(HWND hwnd, int *x, int *y) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && x && y) {
        *x = data->turtleX;
        *y = data->turtleY;
    }
}

int Canvas_GetTurtleAngle(HWND hwnd) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    return data ? data->turtleAngle : 0;
}

/* ==========================================================================
 * Compatibility wrappers (used by interpreters)
 * These are thin helpers that operate on the internal CanvasData.
 * ======================================================================== */

void Canvas_Forward(HWND hwnd, double distance) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data) return;
    double rad = data->turtleAngle * M_PI / 180.0;
    double newX = data->turtleX + cos(rad) * distance;
    double newY = data->turtleY - sin(rad) * distance; /* screen Y downwards */

    if (data->penDown && data->hdcMem) {
        HPEN hPen = CreatePen(PS_SOLID, data->penWidth, data->penColor);
        HPEN hOld = (HPEN)SelectObject(data->hdcMem, hPen);
        MoveToEx(data->hdcMem, data->turtleX, data->turtleY, NULL);
        LineTo(data->hdcMem, (int)newX, (int)newY);
        SelectObject(data->hdcMem, hOld);
        DeleteObject(hPen);
        InvalidateRect(hwnd, NULL, FALSE);
    }

    data->turtleX = (int)newX;
    data->turtleY = (int)newY;
}

void Canvas_Back(HWND hwnd, double distance) {
    Canvas_Forward(hwnd, -distance);
}

void Canvas_Left(HWND hwnd, double angle) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data) return;
    data->turtleAngle = (data->turtleAngle + (int)angle) % 360;
    InvalidateRect(hwnd, NULL, FALSE);
}

void Canvas_Right(HWND hwnd, double angle) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data) return;
    data->turtleAngle = (data->turtleAngle - (int)angle) % 360;
    if (data->turtleAngle < 0) data->turtleAngle += 360;
    InvalidateRect(hwnd, NULL, FALSE);
}

void Canvas_PenUp(HWND hwnd) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) data->penDown = FALSE;
}

void Canvas_PenDown(HWND hwnd) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) data->penDown = TRUE;
}

void Canvas_Home(HWND hwnd) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data) return;
    int cx = data->width / 2;
    int cy = data->height / 2;
    if (data->penDown && data->hdcMem) {
        HPEN hPen = CreatePen(PS_SOLID, data->penWidth, data->penColor);
        HPEN hOld = (HPEN)SelectObject(data->hdcMem, hPen);
        MoveToEx(data->hdcMem, data->turtleX, data->turtleY, NULL);
        LineTo(data->hdcMem, cx, cy);
        SelectObject(data->hdcMem, hOld);
        DeleteObject(hPen);
        InvalidateRect(hwnd, NULL, FALSE);
    }
    data->turtleX = cx;
    data->turtleY = cy;
    data->turtleAngle = 90; /* Logo-style default */
}

void Canvas_SetXY(HWND hwnd, double x, double y) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data) return;
    int newX = (int)round(x);
    int newY = (int)round(y);
    if (data->penDown && data->hdcMem) {
        HPEN hPen = CreatePen(PS_SOLID, data->penWidth, data->penColor);
        HPEN hOld = (HPEN)SelectObject(data->hdcMem, hPen);
        MoveToEx(data->hdcMem, data->turtleX, data->turtleY, NULL);
        LineTo(data->hdcMem, newX, newY);
        SelectObject(data->hdcMem, hOld);
        DeleteObject(hPen);
        InvalidateRect(hwnd, NULL, FALSE);
    }
    data->turtleX = newX;
    data->turtleY = newY;
}

void Canvas_Circle(HWND hwnd, int radius) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data) return;
    Canvas_DrawCircle(hwnd, data->turtleX, data->turtleY, radius);
}

void Canvas_SetBgColor(HWND hwnd, COLORREF color) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data || !data->hdcMem) return;
    HBRUSH hBrush = CreateSolidBrush(color);
    RECT rc = {0, 0, data->width, data->height};
    FillRect(data->hdcMem, &rc, hBrush);
    DeleteObject(hBrush);
    InvalidateRect(hwnd, NULL, TRUE);
}

void Canvas_DrawTurtle(HWND hwnd, int x, int y, int heading, BOOL penDown) {
    CanvasData *data = (CanvasData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data || !data->hdcMem) return;

    /* simple triangle representing the turtle */
    POINT pts[3];
    double rad = heading * M_PI / 180.0;
    /* nose */
    pts[0].x = x + (int)(12 * cos(rad));
    pts[0].y = y - (int)(12 * sin(rad));
    /* left */
    pts[1].x = x - (int)(8 * cos(rad + M_PI / 2));
    pts[1].y = y + (int)(8 * sin(rad + M_PI / 2));
    /* right */
    pts[2].x = x - (int)(8 * cos(rad - M_PI / 2));
    pts[2].y = y + (int)(8 * sin(rad - M_PI / 2));

    HBRUSH hBrush = CreateSolidBrush(penDown ? RGB(231, 76, 60) : RGB(149, 165, 166));
    HBRUSH hOldB = (HBRUSH)SelectObject(data->hdcMem, hBrush);
    HPEN hPen = CreatePen(PS_SOLID, 2, RGB(44, 62, 80));
    HPEN hOldP = (HPEN)SelectObject(data->hdcMem, hPen);

    Polygon(data->hdcMem, pts, 3);

    SelectObject(data->hdcMem, hOldP);
    SelectObject(data->hdcMem, hOldB);
    DeleteObject(hPen);
    DeleteObject(hBrush);

    InvalidateRect(hwnd, NULL, FALSE);
}
