/*
 * forth_interpreter.h - Forth interpreter interface
 * Supports stack operations, dictionary definitions, and basic arithmetic
 */

#ifndef FORTH_INTERPRETER_H
#define FORTH_INTERPRETER_H

#include <windows.h>

/* Interpreter functions */
BOOL ForthInterpreter_Init(void);
void ForthInterpreter_Cleanup(void);
BOOL ForthInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode);
void ForthInterpreter_Stop(void);
void ForthInterpreter_Reset(void);

#endif /* FORTH_INTERPRETER_H */
