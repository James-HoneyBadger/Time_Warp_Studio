/*
 * basic_interpreter.h - Complete BASIC interpreter
 * Supports variables, arrays, loops, conditionals, functions, file I/O
 */

#ifndef BASIC_INTERPRETER_H
#define BASIC_INTERPRETER_H

#include <windows.h>

/* Interpreter functions */
BOOL BasicInterpreter_Init(void);
void BasicInterpreter_Cleanup(void);
BOOL BasicInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode);
void BasicInterpreter_Stop(void);
void BasicInterpreter_Reset(void);
BOOL BasicInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen);
BOOL BasicInterpreter_SetVariable(const TCHAR *name, const TCHAR *value);
int BasicInterpreter_GetLineCount(void);
const TCHAR *BasicInterpreter_GetLine(int lineNum);

#endif /* BASIC_INTERPRETER_H */
