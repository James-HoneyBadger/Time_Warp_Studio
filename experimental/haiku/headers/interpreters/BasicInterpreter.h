/*
 * Time Warp IDE - Haiku OS Edition
 * BASIC interpreter header
 */

#ifndef BASIC_INTERPRETER_H
#define BASIC_INTERPRETER_H

#include <String.h>
#include <map>
#include <vector>

class BasicInterpreter
{
public:
    BasicInterpreter();
    ~BasicInterpreter();

    BString Execute(const char *command);
    BString RunProgram(const char *program);
    void Reset();

private:
    std::map<BString, float> fVariables;
    std::vector<BString> fProgramLines;
    int fCurrentLine;

    BString _ExecutePrint(const char *args);
    BString _ExecuteLet(const char *args);
    BString _ExecuteInput(const char *args);
    BString _ExecuteGoto(const char *args);
    BString _ExecuteIf(const char *args);
    float _EvaluateExpression(const char *expr);
};

#endif // BASIC_INTERPRETER_H
