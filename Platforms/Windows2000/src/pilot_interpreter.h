/*
 * pilot_interpreter.h - Complete PILOT interpreter
 * Supports all commands: T:, A:, M:, C:, pattern matching, branching
 * Conditional commands: TY:, TN:, etc.
 * Variable interpolation: #VAR and *VAR* syntax
 * Graphics commands: G:
 * Subroutine calls: S:, R:
 */

#ifndef PILOT_INTERPRETER_H
#define PILOT_INTERPRETER_H

#include <windows.h>

/* Interpreter functions */
BOOL PilotInterpreter_Init(void);
void PilotInterpreter_Cleanup(void);
BOOL PilotInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode);
void PilotInterpreter_Stop(void);
void PilotInterpreter_Reset(void);
BOOL PilotInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen);
BOOL PilotInterpreter_SetVariable(const TCHAR *name, const TCHAR *value);
void PilotInterpreter_SetLastInput(const TCHAR *input);

#endif /* PILOT_INTERPRETER_H */
