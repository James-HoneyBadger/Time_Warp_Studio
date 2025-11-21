/*
 * debugger.h - Full debugger with breakpoints, stepping, watch windows
 * Supports step into/over/out, watch variables, call stack, memory view
 */

#ifndef DEBUGGER_H
#define DEBUGGER_H

#include <windows.h>

/* Debugger functions */
BOOL Debugger_Init(void);
void Debugger_Cleanup(void);
HWND Debugger_CreateWindow(HWND hwndParent, HINSTANCE hInstance);
void Debugger_Step(void);
void Debugger_Continue(void);
void Debugger_StepOver(void);
void Debugger_StepOut(void);
void Debugger_ToggleBreakpoint(int line);
BOOL Debugger_IsBreakpoint(int line);
void Debugger_ClearAllBreakpoints(void);
void Debugger_ShowWatchWindow(void);
void Debugger_ShowCallStackWindow(void);
void Debugger_ShowMemoryWindow(void);
void Debugger_AddWatch(const TCHAR *expression);
void Debugger_RemoveWatch(int index);
int Debugger_GetCallStackDepth(void);
const TCHAR *Debugger_GetCallStackFrame(int depth);
void Debugger_UpdateState(int currentLine);

#endif /* DEBUGGER_H */
