/*
 * basic_interpreter.c - BASIC interpreter
 */

#include "basic_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
    TCHAR name[32];
    double value;
} Variable;

static Variable g_variables[256];
static int g_varCount = 0;
static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static HWND g_hwndCanvas = NULL;

BOOL BasicInterpreter_Init(void) {
    g_varCount = 0;
    g_running = FALSE;
    return TRUE;
}

void BasicInterpreter_Cleanup(void) {
    g_varCount = 0;
}

static void Basic_Print(const TCHAR *text) {
    if (g_hwndConsole) {
        TCHAR buf[1024];
        _stprintf(buf, TEXT("%s\r\n"), text);
        int len = GetWindowTextLength(g_hwndConsole);
        SendMessage(g_hwndConsole, EM_SETSEL, len, len);
        SendMessage(g_hwndConsole, EM_REPLACESEL, FALSE, (LPARAM)buf);
    }
}

BOOL BasicInterpreter_Execute(const TCHAR *code, HWND hwndConsole, HWND hwndCanvas, BOOL debugMode) {
    g_hwndConsole = hwndConsole;
    g_hwndCanvas = hwndCanvas;
    g_running = TRUE;
    
    TCHAR *line = _tcsdup(code);
    TCHAR *tok = _tcstok(line, TEXT("\n"));
    
    while (tok && g_running) {
        if (_tcsnicmp(tok, TEXT("PRINT"), 5) == 0) {
            Basic_Print(tok + 6);
        } else if (_tcsnicmp(tok, TEXT("CLS"), 3) == 0) {
            if (g_hwndCanvas) Canvas_Clear(g_hwndCanvas);
        }
        tok = _tcstok(NULL, TEXT("\n"));
    }
    
    free(line);
    g_running = FALSE;
    return TRUE;
}

void BasicInterpreter_Stop(void) {
    g_running = FALSE;
}

void BasicInterpreter_Reset(void) {
    g_varCount = 0;
}

BOOL BasicInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) {
    for (int i = 0; i < g_varCount; i++) {
        if (_tcsicmp(g_variables[i].name, name) == 0) {
            _stprintf(value, TEXT("%g"), g_variables[i].value);
            return TRUE;
        }
    }
    return FALSE;
}

BOOL BasicInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) {
    for (int i = 0; i < g_varCount; i++) {
        if (_tcsicmp(g_variables[i].name, name) == 0) {
            g_variables[i].value = _tstof(value);
            return TRUE;
        }
    }
    if (g_varCount < 256) {
        _tcscpy(g_variables[g_varCount].name, name);
        g_variables[g_varCount].value = _tstof(value);
        g_varCount++;
        return TRUE;
    }
    return FALSE;
}

int BasicInterpreter_GetLineCount(void) { return 0; }
const TCHAR *BasicInterpreter_GetLine(int lineNum) { return TEXT(""); }
