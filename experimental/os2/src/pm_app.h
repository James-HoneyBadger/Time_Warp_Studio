#ifndef PM_APP_H
#define PM_APP_H

#define INCL_WIN
#define INCL_GPIPRIMITIVES
#include <os2.h>

extern HAB hab;
extern HMQ hmq;
extern HWND hwndFrame;
extern HWND hwndClient;

#define ID_TIMER_STATUS 101

// Menu IDs
#define IDM_FILE_OPEN 2001
#define IDM_FILE_EXIT 2002
#define IDM_RUN_EXEC 2101
#define IDM_RUN_STOP 2102
#define IDM_LANG_BASIC 2201
#define IDM_LANG_LOGO 2202
#define IDM_LANG_PILOT 2203

// Child window IDs
#define IDW_CONSOLE 3001
#define IDW_CANVAS 3002

// Selected language enum
typedef enum
{
    LANG_BASIC,
    LANG_LOGO,
    LANG_PILOT
} TW_LANG;

extern TW_LANG g_current_lang;

HWND create_frame_window(void);
MRESULT EXPENTRY client_wnd_proc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2);
void tw_append_console(const char *text);
void tw_canvas_draw_line(LONG x1, LONG y1, LONG x2, LONG y2);
void tw_canvas_clear(void);
void tw_run_program(const char *source);

#endif // PM_APP_H