/*
 * pilot_interpreter.h - OS/2 PILOT interpreter header
 */

#ifndef PILOT_INTERPRETER_H
#define PILOT_INTERPRETER_H

#define INCL_PM
#include <os2.h>

BOOL PilotInterpreter_Init(void);
void PilotInterpreter_Cleanup(void);
BOOL PilotInterpreter_Execute(const char *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode);
void PilotInterpreter_Stop(void);
void PilotInterpreter_Reset(void);

#endif /* PILOT_INTERPRETER_H */
