/*
 * Time Warp IDE - Haiku OS Edition
 * PILOT interpreter header (stub)
 */

#ifndef PILOT_INTERPRETER_H
#define PILOT_INTERPRETER_H

#include <String.h>

class PilotInterpreter
{
public:
    PilotInterpreter();
    ~PilotInterpreter();

    BString Execute(const char *command);
    BString RunProgram(const char *program);
    void Reset();
};

#endif // PILOT_INTERPRETER_H
