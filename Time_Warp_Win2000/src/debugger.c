/*
 * debugger.c - Debugger implementation
 */

#include "debugger.h"
#include <stdio.h>
#include <stdlib.h>

static BOOL g_breakpoints[1024];
static int g_currentLine = 0;

BOOL Debugger_Init(void) {
    memset(g_breakpoints, 0, sizeof(g_breakpoints));
    return TRUE;
}

void Debugger_Cleanup(void) { }

HWND Debugger_CreateWindow(HWND hwndParent, HINSTANCE hInstance) {
    return CreateWindowEx(0, TEXT("STATIC"), TEXT("Debugger"),
                         WS_CHILD | WS_BORDER, 0, 0, 0, 0,
                         hwndParent, NULL, hInstance, NULL);
}

void Debugger_Step(void) { g_currentLine++; }
void Debugger_Continue(void) { }
void Debugger_StepOver(void) { g_currentLine++; }
void Debugger_StepOut(void) { }

void Debugger_ToggleBreakpoint(int line) {
    if (line >= 0 && line < 1024) {
        g_breakpoints[line] = !g_breakpoints[line];
    }
}

BOOL Debugger_IsBreakpoint(int line) {
    return (line >= 0 && line < 1024) ? g_breakpoints[line] : FALSE;
}

void Debugger_ClearAllBreakpoints(void) {
    memset(g_breakpoints, 0, sizeof(g_breakpoints));
}

void Debugger_ShowWatchWindow(void) { }
void Debugger_ShowCallStackWindow(void) { }
void Debugger_ShowMemoryWindow(void) { }
void Debugger_AddWatch(const TCHAR *expression) { }
void Debugger_RemoveWatch(int index) { }
int Debugger_GetCallStackDepth(void) { return 0; }
const TCHAR *Debugger_GetCallStackFrame(int depth) { return TEXT(""); }
void Debugger_UpdateState(int currentLine) { g_currentLine = currentLine; }
