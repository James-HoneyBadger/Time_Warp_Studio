/*
 * basic_interpreter.h - OS/2 BASIC interpreter header
 * Comprehensive BASIC implementation for OS/2 Presentation Manager
 */

#ifndef BASIC_INTERPRETER_H
#define BASIC_INTERPRETER_H

#define INCL_PM
#define INCL_GPI
#include <os2.h>

/* Interpreter functions */
BOOL BasicInterpreter_Init(void);
void BasicInterpreter_Cleanup(void);
BOOL BasicInterpreter_Execute(const char *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode);
void BasicInterpreter_Stop(void);
void BasicInterpreter_Reset(void);
BOOL BasicInterpreter_GetVariable(const char *name, char *value, int maxLen);
BOOL BasicInterpreter_SetVariable(const char *name, const char *value);
int BasicInterpreter_GetLineCount(void);
const char *BasicInterpreter_GetLine(int lineNum);

#endif /* BASIC_INTERPRETER_H */
