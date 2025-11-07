#define INCL_WIN
#define INCL_GPIPRIMITIVES
#include <os2.h>
#include <string.h>
#include <stdlib.h>
#include "pm_app.h"
#include <math.h>

HAB hab = 0;
HMQ hmq = 0;
HWND hwndFrame = 0;
HWND hwndClient = 0;
HWND hwndConsole = 0;
HWND hwndCanvas = 0;
TW_LANG g_current_lang = LANG_BASIC;

static CHAR g_source_buffer[65536];
static LONG turtle_x = 320, turtle_y = 240, turtle_angle = 0;

HWND create_frame_window(void)
{
    ULONG flFrameFlags = FCF_TITLEBAR | FCF_SYSMENU | FCF_SIZEBORDER | FCF_MINMAX | FCF_SHELLPOSITION | FCF_MENU | FCF_ACCELTABLE;
    hwndFrame = WinCreateStdWindow(HWND_DESKTOP, 0, &flFrameFlags, "TWCLIENT", "Time Warp OS/2", 0, NULLHANDLE, 1, &hwndClient);
    return hwndFrame;
}

static void create_children(void)
{
    hwndConsole = WinCreateWindow(hwndClient, WC_MLE, "", MLS_WORDWRAP, 0, 0, 400, 480, hwndClient, HWND_TOP, IDW_CONSOLE, NULL, NULL);
    hwndCanvas = WinCreateWindow(hwndClient, WC_STATIC, "", SS_TEXT, 400, 0, 240, 480, hwndClient, HWND_TOP, IDW_CANVAS, NULL, NULL);
}

void tw_append_console(const char *text)
{
    if (!hwndConsole)
        return;
    ULONG len = strlen(text);
    MLESETTEXT mset;
    // Simplified append: query length then insert
    ULONG curLen = (ULONG)WinSendMsg(hwndConsole, MLM_QUERYTEXTLENGTH, 0, 0);
    WinSendMsg(hwndConsole, MLM_SETSEL, MPFROMLONG(curLen), MPFROMLONG(curLen));
    WinSendMsg(hwndConsole, MLM_INSERT, MPFROMP((PSZ)text), MPFROMLONG(len));
}

void tw_canvas_clear(void)
{
    WinInvalidateRect(hwndCanvas, NULL, FALSE);
}

void tw_canvas_draw_line(LONG x1, LONG y1, LONG x2, LONG y2)
{
    RECTL rcl;
    WinQueryWindowRect(hwndCanvas, &rcl);
    HPS hps = WinGetPS(hwndCanvas);
    GpiMove(hps, &(POINTL){x1, y1});
    GpiLine(hps, &(POINTL){x2, y2});
    WinReleasePS(hps);
}

static void turtle_forward(LONG dist)
{
    double rad = (double)turtle_angle * 3.1415926535 / 180.0;
    LONG nx = turtle_x + (LONG)(dist * cos(rad));
    LONG ny = turtle_y + (LONG)(dist * sin(rad));
    tw_canvas_draw_line(turtle_x, turtle_y, nx, ny);
    turtle_x = nx;
    turtle_y = ny;
}

static void turtle_right(LONG deg) { turtle_angle = (turtle_angle + deg) % 360; }
static void turtle_cs(void)
{
    turtle_x = 320;
    turtle_y = 240;
    turtle_angle = 0;
    tw_canvas_clear();
}

void tw_run_program(const char *source)
{
    strncpy(g_source_buffer, source, sizeof(g_source_buffer) - 1);
    g_source_buffer[sizeof(g_source_buffer) - 1] = '\0';
    tw_append_console("🚀 Running program...\r\n");
    char line[256];
    const char *p = g_source_buffer;
    while (*p)
    {
        int i = 0;
        while (*p && *p != '\n' && i < 255)
            line[i++] = *p++;
        if (*p == '\n')
            p++;
        line[i] = '\0';
        if (g_current_lang == LANG_BASIC)
        {
            if (strncmp(line, "PRINT", 5) == 0)
            {
                const char *msg = line + 5;
                while (*msg == ' ' || *msg == '\t')
                    msg++;
                tw_append_console(msg);
                tw_append_console("\r\n");
            }
            else if (strcmp(line, "CLS") == 0)
            {
                WinSendMsg(hwndConsole, MLM_DELETE, MPFROMLONG(0), MPFROMLONG((ULONG)WinSendMsg(hwndConsole, MLM_QUERYTEXTLENGTH, 0, 0)));
            }
        }
        else if (g_current_lang == LANG_LOGO)
        {
            if (strncmp(line, "FORWARD", 7) == 0)
            {
                int d = atoi(line + 7);
                turtle_forward(d);
            }
            else if (strncmp(line, "RIGHT", 5) == 0)
            {
                int d = atoi(line + 5);
                turtle_right(d);
            }
            else if (strcmp(line, "CS") == 0)
            {
                turtle_cs();
            }
        }
        else if (g_current_lang == LANG_PILOT)
        {
            if (strncmp(line, "T:", 2) == 0)
            {
                const char *msg = line + 2;
                while (*msg == ' ' || *msg == '\t')
                    msg++;
                tw_append_console(msg);
                tw_append_console("\r\n");
            }
            else if (strcmp(line, "END") == 0)
            {
                tw_append_console("ℹ️ End of PILOT script\r\n");
            }
        }
    }
    tw_append_console("✅ Done\r\n");
}

MRESULT EXPENTRY client_wnd_proc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
    switch (msg)
    {
    case WM_CREATE:
        create_children();
        return 0;
    case WM_COMMAND:
        switch (SHORT1FROMMP(mp1))
        {
        case IDM_LANG_BASIC:
            g_current_lang = LANG_BASIC;
            tw_append_console("ℹ️ BASIC selected\r\n");
            break;
        case IDM_LANG_LOGO:
            g_current_lang = LANG_LOGO;
            tw_append_console("ℹ️ LOGO selected\r\n");
            break;
        case IDM_LANG_PILOT:
            g_current_lang = LANG_PILOT;
            tw_append_console("ℹ️ PILOT selected\r\n");
            break;
        case IDM_RUN_EXEC:
        {
            const char *demo = (g_current_lang == LANG_BASIC) ? "PRINT Hello OS/2!\n" : (g_current_lang == LANG_LOGO) ? "FORWARD 50\nRIGHT 90\nFORWARD 50\n"
                                                                                                                      : "T:Welcome to PILOT\nEND\n";
            tw_run_program(demo);
        }
        break;
        case IDM_FILE_EXIT:
            WinPostMsg(hwndFrame, WM_QUIT, 0, 0);
            break;
        }
        return 0;
    case WM_PAINT:
    {
        HPS hps = WinBeginPaint(hwnd, NULLHANDLE, NULL);
        RECTL rcl;
        WinQueryWindowRect(hwnd, &rcl);
        WinFillRect(hps, &rcl, CLR_WHITE);
        WinEndPaint(hps);
        return 0;
    }
    }
    return WinDefWindowProc(hwnd, msg, mp1, mp2);
}