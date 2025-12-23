/*
 * pascal_interpreter.h - Pascal interpreter interface
 * Supports basic Pascal syntax: program, var, begin, end, writeln
 */

#ifndef PASCAL_INTERPRETER_H
#define PASCAL_INTERPRETER_H

#include <windows.h>

/* Interpreter functions */
BOOL PascalInterpreter_Init(void);
void PascalInterpreter_Cleanup(void);
BOOL PascalInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                               HWND hwndCanvas, BOOL debugMode);
void PascalInterpreter_Stop(void);
void PascalInterpreter_Reset(void);

#endif /* PASCAL_INTERPRETER_H */
