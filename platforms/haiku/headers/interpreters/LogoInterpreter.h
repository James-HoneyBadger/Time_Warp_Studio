/*
 * Time Warp IDE - Haiku OS Edition
 * Logo interpreter header (stub)
 */

#ifndef LOGO_INTERPRETER_H
#define LOGO_INTERPRETER_H

#include <String.h>

class LogoInterpreter
{
public:
    LogoInterpreter();
    ~LogoInterpreter();

    BString Execute(const char *command);
    BString RunProgram(const char *program);
    void Reset();

    // Turtle state accessors for TurtleView integration
    float GetTurtleX() const { return fTurtleX; }
    float GetTurtleY() const { return fTurtleY; }
    float GetTurtleAngle() const { return fTurtleAngle; }

private:
    float fTurtleX;
    float fTurtleY;
    float fTurtleAngle;
    bool fPenDown;
};

#endif // LOGO_INTERPRETER_H
