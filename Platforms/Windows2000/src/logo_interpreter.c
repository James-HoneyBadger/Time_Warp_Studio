/*
 * logo_interpreter.c - Logo interpreter
 */

#include "logo_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <tchar.h>
#include "canvas.h"

#ifndef _MSC_VER
#ifndef _tcstok_s
#define _tcstok_s(str, delim, ctx) _tcstok(str, delim)
#endif
#ifndef _tcsdup
#define _tcsdup _strdup
#endif
#ifndef _tcsncat
#define _tcsncat strncat
#endif
/* Leave existing _tcsnicmp implementation from toolchain */
#endif

static BOOL g_running = FALSE;
static HWND g_hwndCanvas = NULL;
static int g_turtleX = 512;
static int g_turtleY = 384;
static int g_turtleAngle = 0;
static BOOL g_penDown = TRUE;

typedef struct
{
    TCHAR name[32];
    TCHAR body[4096];
} LogoProc;

static LogoProc g_procs[64];
static int g_procCount = 0;

static int find_proc(const TCHAR *name)
{
    for (int i = 0; i < g_procCount; i++)
    {
        if (_tcsicmp(g_procs[i].name, name) == 0)
            return i;
    }
    return -1;
}

static void store_proc(const TCHAR *name, const TCHAR *body)
{
    int idx = find_proc(name);
    if (idx < 0 && g_procCount < 64)
    {
        idx = g_procCount++;
    }
    if (idx >= 0)
    {
        /* Safe: buffers are declared with explicit sizes; we null-terminate */
        size_t name_len = strlen(name);
        size_t body_len = strlen(body);
        if (name_len > 31) name_len = 31;
        if (body_len > 4095) body_len = 4095;
        memcpy(g_procs[idx].name, name, name_len);
        g_procs[idx].name[name_len] = 0;
        memcpy(g_procs[idx].body, body, body_len);
        g_procs[idx].body[body_len] = 0;
    }
}

static BOOL execute_logo_block(const TCHAR *code, int depth);

BOOL LogoInterpreter_Init(void)
{
    g_running = FALSE;
    return TRUE;
}

void LogoInterpreter_Cleanup(void) {}

static void Logo_Forward(int distance)
{
    if (g_hwndCanvas)
    {
        double rad = g_turtleAngle * M_PI / 180.0;
        int newX = g_turtleX + (int)(distance * cos(rad));
        int newY = g_turtleY + (int)(distance * sin(rad));
        if (g_penDown)
        {
            Canvas_MoveTo(g_hwndCanvas, g_turtleX, g_turtleY);
            Canvas_LineTo(g_hwndCanvas, newX, newY);
        }
        g_turtleX = newX;
        g_turtleY = newY;
    }
}

static void Logo_Right(int angle)
{
    g_turtleAngle = (g_turtleAngle + angle) % 360;
}

static void Logo_Left(int angle)
{
    g_turtleAngle = (g_turtleAngle - angle) % 360;
    if (g_turtleAngle < 0)
        g_turtleAngle += 360;
}

static void Logo_SetXY(int x, int y)
{
    if (g_hwndCanvas)
    {
        if (g_penDown)
        {
            Canvas_MoveTo(g_hwndCanvas, g_turtleX, g_turtleY);
            Canvas_LineTo(g_hwndCanvas, x, y);
        }
    }
    g_turtleX = x;
    g_turtleY = y;
}

static void Logo_Back(int distance)
{
    Logo_Forward(-distance);
}

static void Logo_SetPenColorRGB(int r, int g, int b)
{
    if (g_hwndCanvas)
        Canvas_SetPenColor(g_hwndCanvas, RGB(r, g, b));
}

static void Logo_SetPenWidth(int w)
{
    if (g_hwndCanvas)
        Canvas_SetPenWidth(g_hwndCanvas, w > 0 ? w : 1);
}

BOOL LogoInterpreter_Execute(const TCHAR *code, HWND hwndConsole, HWND hwndCanvas, BOOL debugMode)
{
    g_hwndCanvas = hwndCanvas;
    g_running = TRUE;

    // First pass: collect TO ... END procedures
    {
        TCHAR *dup = _tcsdup(code);
        TCHAR *save = NULL;
        TCHAR *line = _tcstok_s(dup, TEXT("\n"), &save);
        while (line)
        {
            while (*line == TEXT(' ') || *line == TEXT('\t'))
                line++;
            if (_tcsnicmp(line, TEXT("TO "), 3) == 0)
            {
                const TCHAR *name = line + 3;
                while (*name == TEXT(' ') || *name == TEXT('\t'))
                    name++;
                TCHAR pname[32] = {0};
                int i = 0;
                while (*name && !isspace((unsigned char)*name) && i < 31)
                {
                    pname[i++] = *name++;
                }
                TCHAR body[4096] = {0};
                TCHAR *cur = _tcstok_s(NULL, TEXT("\n"), &save);
                while (cur)
                {
                    TCHAR *trim = cur;
                    while (*trim == TEXT(' ') || *trim == TEXT('\t'))
                        trim++;
                    if (_tcsicmp(trim, TEXT("END")) == 0)
                        break;
                    _tcsncat(body, cur, 4095 - _tcslen(body));
                    _tcsncat(body, TEXT("\n"), 4095 - _tcslen(body));
                    cur = _tcstok_s(NULL, TEXT("\n"), &save);
                }
                store_proc(pname, body);
                line = _tcstok_s(NULL, TEXT("\n"), &save);
                continue;
            }
            line = _tcstok_s(NULL, TEXT("\n"), &save);
        }
        free(dup);
    }

    TCHAR *line = _tcsdup(code);
    TCHAR *tok = _tcstok(line, TEXT(" \n"));

    while (tok && g_running)
    {
        if (_tcsicmp(tok, TEXT("FORWARD")) == 0 || _tcsicmp(tok, TEXT("FD")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Forward(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("RIGHT")) == 0 || _tcsicmp(tok, TEXT("RT")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Right(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("LEFT")) == 0 || _tcsicmp(tok, TEXT("LT")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Left(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("BACK")) == 0 || _tcsicmp(tok, TEXT("BK")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Back(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("PENUP")) == 0 || _tcsicmp(tok, TEXT("PU")) == 0)
        {
            g_penDown = FALSE;
        }
        else if (_tcsicmp(tok, TEXT("PENDOWN")) == 0 || _tcsicmp(tok, TEXT("PD")) == 0)
        {
            g_penDown = TRUE;
        }
        else if (_tcsicmp(tok, TEXT("HOME")) == 0)
        {
            Logo_SetXY(512, 384);
            g_turtleAngle = 0;
        }
        else if (_tcsicmp(tok, TEXT("CS")) == 0)
        {
            if (g_hwndCanvas)
                Canvas_Clear(g_hwndCanvas);
            Logo_SetXY(512, 384);
            g_turtleAngle = 0;
            g_penDown = TRUE;
        }
        else if (_tcsicmp(tok, TEXT("SETXY")) == 0)
        {
            // Expect two integers
            TCHAR *xTok = _tcstok(NULL, TEXT(" \n"));
            TCHAR *yTok = _tcstok(NULL, TEXT(" \n"));
            if (xTok && yTok)
                Logo_SetXY(_ttoi(xTok), _ttoi(yTok));
        }
        else if (_tcsicmp(tok, TEXT("SETH")) == 0 || _tcsicmp(tok, TEXT("SETHEADING")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
            {
                g_turtleAngle = _ttoi(tok) % 360;
                if (g_turtleAngle < 0)
                    g_turtleAngle += 360;
            }
        }
        else if (_tcsicmp(tok, TEXT("SETCOLOR")) == 0 || _tcsicmp(tok, TEXT("SETPENCOLOR")) == 0)
        {
            TCHAR *rTok = _tcstok(NULL, TEXT(" \n"));
            TCHAR *gTok = _tcstok(NULL, TEXT(" \n"));
            TCHAR *bTok = _tcstok(NULL, TEXT(" \n"));
            if (rTok && gTok && bTok)
                Logo_SetPenColorRGB(_ttoi(rTok), _ttoi(gTok), _ttoi(bTok));
        }
        else if (_tcsicmp(tok, TEXT("PENWIDTH")) == 0 || _tcsicmp(tok, TEXT("SETPENSIZE")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_SetPenWidth(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("TO")) == 0)
        {
            while (tok && _tcsicmp(tok, TEXT("END")) != 0)
                tok = _tcstok(NULL, TEXT(" \n"));
        }
        else
        {
            int idx = find_proc(tok);
            if (idx >= 0)
            {
                execute_logo_block(g_procs[idx].body, 0);
            }
        }
        tok = _tcstok(NULL, TEXT(" \n"));
    }

    free(line);
    g_running = FALSE;
    return TRUE;
}

void LogoInterpreter_Stop(void) { g_running = FALSE; }
void LogoInterpreter_Reset(void)
{
    g_turtleX = 512;
    g_turtleY = 384;
    g_turtleAngle = 0;
}
BOOL LogoInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) { return FALSE; }
BOOL LogoInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) { return FALSE; }
BOOL LogoInterpreter_DefineProcedure(const TCHAR *name, const TCHAR *body) { return TRUE; }
BOOL LogoInterpreter_CallProcedure(const TCHAR *name, const TCHAR **args, int argCount) { return TRUE; }

static BOOL execute_logo_block(const TCHAR *code, int depth)
{
    if (depth > 16)
        return FALSE;
    TCHAR *dup = _tcsdup(code);
    TCHAR *tok = _tcstok(dup, TEXT(" \n"));
    while (tok && g_running)
    {
        if (_tcsicmp(tok, TEXT("FORWARD")) == 0 || _tcsicmp(tok, TEXT("FD")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Forward(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("BACK")) == 0 || _tcsicmp(tok, TEXT("BK")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Back(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("RIGHT")) == 0 || _tcsicmp(tok, TEXT("RT")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Right(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("LEFT")) == 0 || _tcsicmp(tok, TEXT("LT")) == 0)
        {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok)
                Logo_Left(_ttoi(tok));
        }
        else if (_tcsicmp(tok, TEXT("PU")) == 0 || _tcsicmp(tok, TEXT("PENUP")) == 0)
        {
            g_penDown = FALSE;
        }
        else if (_tcsicmp(tok, TEXT("PD")) == 0 || _tcsicmp(tok, TEXT("PENDOWN")) == 0)
        {
            g_penDown = TRUE;
        }
        else if (_tcsicmp(tok, TEXT("SETXY")) == 0)
        {
            TCHAR *xTok = _tcstok(NULL, TEXT(" \n"));
            TCHAR *yTok = _tcstok(NULL, TEXT(" \n"));
            if (xTok && yTok)
                Logo_SetXY(_ttoi(xTok), _ttoi(yTok));
        }
        else if (_tcsicmp(tok, TEXT("SETH")) == 0 || _tcsicmp(tok, TEXT("SETHEADING")) == 0)
        {
            TCHAR *aTok = _tcstok(NULL, TEXT(" \n"));
            if (aTok)
            {
                g_turtleAngle = _ttoi(aTok) % 360;
                if (g_turtleAngle < 0)
                    g_turtleAngle += 360;
            }
        }
        else if (_tcsicmp(tok, TEXT("SETCOLOR")) == 0 || _tcsicmp(tok, TEXT("SETPENCOLOR")) == 0)
        {
            TCHAR *rTok = _tcstok(NULL, TEXT(" \n"));
            TCHAR *gTok = _tcstok(NULL, TEXT(" \n"));
            TCHAR *bTok = _tcstok(NULL, TEXT(" \n"));
            if (rTok && gTok && bTok)
                Logo_SetPenColorRGB(_ttoi(rTok), _ttoi(gTok), _ttoi(bTok));
        }
        else
        {
            int idx = find_proc(tok);
            if (idx >= 0)
            {
                execute_logo_block(g_procs[idx].body, depth + 1);
            }
        }
        tok = _tcstok(NULL, TEXT(" \n"));
    }
    free(dup);
    return TRUE;
}
