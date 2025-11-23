/*
 * pilot_interpreter.c - PILOT interpreter
 */

#include "pilot_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;

BOOL PilotInterpreter_Init(void) {
    g_running = FALSE;
    return TRUE;
}

void PilotInterpreter_Cleanup(void) { }

static void Pilot_Type(const TCHAR *text) {
    if (g_hwndConsole) {
        TCHAR buf[1024];
        _stprintf(buf, TEXT("%s\r\n"), text);
        int len = GetWindowTextLength(g_hwndConsole);
        SendMessage(g_hwndConsole, EM_SETSEL, len, len);
        SendMessage(g_hwndConsole, EM_REPLACESEL, FALSE, (LPARAM)buf);
    }
}

BOOL PilotInterpreter_Execute(const TCHAR *code, HWND hwndConsole, HWND hwndCanvas, BOOL debugMode) {
    g_hwndConsole = hwndConsole;
    g_running = TRUE;
    
    TCHAR *line = _tcsdup(code);
    TCHAR *tok = _tcstok(line, TEXT("\n"));
    
    while (tok && g_running) {
        if (_tcsnicmp(tok, TEXT("T:"), 2) == 0) {
            Pilot_Type(tok + 2);
        } else if (_tcsnicmp(tok, TEXT("END"), 3) == 0) {
            break;
        }
        tok = _tcstok(NULL, TEXT("\n"));
    }
    
    free(line);
    g_running = FALSE;
    return TRUE;
}

void PilotInterpreter_Stop(void) {
    g_running = FALSE;
}

void PilotInterpreter_Reset(void) { }
BOOL PilotInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) { return FALSE; }
BOOL PilotInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) { return FALSE; }
