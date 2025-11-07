/*
 * logo_interpreter.c - Logo interpreter
 */

#include "logo_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static BOOL g_running = FALSE;
static HWND g_hwndCanvas = NULL;
static int g_turtleX = 512;
static int g_turtleY = 384;
static int g_turtleAngle = 0;

BOOL LogoInterpreter_Init(void) {
    g_running = FALSE;
    return TRUE;
}

void LogoInterpreter_Cleanup(void) { }

static void Logo_Forward(int distance) {
    if (g_hwndCanvas) {
        double rad = g_turtleAngle * M_PI / 180.0;
        int newX = g_turtleX + (int)(distance * cos(rad));
        int newY = g_turtleY + (int)(distance * sin(rad));
        Canvas_MoveTo(g_hwndCanvas, g_turtleX, g_turtleY);
        Canvas_LineTo(g_hwndCanvas, newX, newY);
        g_turtleX = newX;
        g_turtleY = newY;
    }
}

static void Logo_Right(int angle) {
    g_turtleAngle = (g_turtleAngle + angle) % 360;
}

BOOL LogoInterpreter_Execute(const TCHAR *code, HWND hwndConsole, HWND hwndCanvas, BOOL debugMode) {
    g_hwndCanvas = hwndCanvas;
    g_running = TRUE;
    
    TCHAR *line = _tcsdup(code);
    TCHAR *tok = _tcstok(line, TEXT(" \n"));
    
    while (tok && g_running) {
        if (_tcsicmp(tok, TEXT("FORWARD")) == 0 || _tcsicmp(tok, TEXT("FD")) == 0) {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok) Logo_Forward(_ttoi(tok));
        } else if (_tcsicmp(tok, TEXT("RIGHT")) == 0 || _tcsicmp(tok, TEXT("RT")) == 0) {
            tok = _tcstok(NULL, TEXT(" \n"));
            if (tok) Logo_Right(_ttoi(tok));
        } else if (_tcsicmp(tok, TEXT("CS")) == 0) {
            if (g_hwndCanvas) Canvas_Clear(g_hwndCanvas);
        }
        tok = _tcstok(NULL, TEXT(" \n"));
    }
    
    free(line);
    g_running = FALSE;
    return TRUE;
}

void LogoInterpreter_Stop(void) { g_running = FALSE; }
void LogoInterpreter_Reset(void) { g_turtleX = 512; g_turtleY = 384; g_turtleAngle = 0; }
BOOL LogoInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) { return FALSE; }
BOOL LogoInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) { return FALSE; }
BOOL LogoInterpreter_DefineProcedure(const TCHAR *name, const TCHAR *body) { return TRUE; }
BOOL LogoInterpreter_CallProcedure(const TCHAR *name, const TCHAR **args, int argCount) { return TRUE; }
