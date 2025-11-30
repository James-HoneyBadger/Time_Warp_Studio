/*
 * logo_interpreter.h - Complete Logo interpreter
 * Supports all turtle commands, procedures, recursion, variables
 */

#ifndef LOGO_INTERPRETER_H
#define LOGO_INTERPRETER_H

#include <windows.h>

/* Interpreter functions */
BOOL LogoInterpreter_Init(void);
void LogoInterpreter_Cleanup(void);
BOOL LogoInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                             HWND hwndCanvas, BOOL debugMode);
void LogoInterpreter_Stop(void);
void LogoInterpreter_Reset(void);
BOOL LogoInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen);
BOOL LogoInterpreter_SetVariable(const TCHAR *name, const TCHAR *value);
BOOL LogoInterpreter_DefineProcedure(const TCHAR *name, const TCHAR *body);
BOOL LogoInterpreter_CallProcedure(const TCHAR *name, const TCHAR **args, int argCount);

#endif /* LOGO_INTERPRETER_H */
