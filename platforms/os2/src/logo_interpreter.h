/*
 * logo_interpreter.h - OS/2 Logo interpreter header
 */

#ifndef LOGO_INTERPRETER_H
#define LOGO_INTERPRETER_H

#define INCL_PM
#define INCL_GPI
#include <os2.h>

BOOL LogoInterpreter_Init(void);
void LogoInterpreter_Cleanup(void);
BOOL LogoInterpreter_Execute(const char *code, HWND hwndConsole,
                             HWND hwndCanvas, BOOL debugMode);
void LogoInterpreter_Stop(void);
void LogoInterpreter_Reset(void);

#endif /* LOGO_INTERPRETER_H */
