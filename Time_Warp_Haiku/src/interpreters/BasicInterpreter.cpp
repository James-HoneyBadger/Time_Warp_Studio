/*
 * Time Warp IDE - Haiku OS Edition
 * BASIC interpreter stub implementation
 */

#include "BasicInterpreter.h"
#include <cstdlib>
#include <cstring>

BasicInterpreter::BasicInterpreter()
    : fCurrentLine(0)
{
}

BasicInterpreter::~BasicInterpreter()
{
}

void BasicInterpreter::Reset()
{
    fVariables.clear();
    fProgramLines.clear();
    fCurrentLine = 0;
}

BString BasicInterpreter::Execute(const char *command)
{
    BString cmd(command);
    cmd.ToUpper();
    cmd.Trim();

    if (cmd.StartsWith("PRINT "))
    {
        return _ExecutePrint(command + 6);
    }
    else if (cmd.StartsWith("LET "))
    {
        return _ExecuteLet(command + 4);
    }
    else if (cmd.StartsWith("INPUT "))
    {
        return _ExecuteInput(command + 6);
    }
    else if (cmd.StartsWith("GOTO "))
    {
        return _ExecuteGoto(command + 5);
    }
    else if (cmd.StartsWith("IF "))
    {
        return _ExecuteIf(command + 3);
    }
    else if (cmd == "CLS")
    {
        return "🎨 Screen cleared\n";
    }
    else if (cmd == "END")
    {
        return "✅ END\n";
    }
    else
    {
        BString result("❌ BASIC: unknown command '");
        result << command << "'\n";
        return result;
    }
}

BString BasicInterpreter::RunProgram(const char *program)
{
    BString output("🚀 Running BASIC program...\n");

    // TODO: Parse and execute multi-line programs
    // For now, just execute as single command
    output << Execute(program);

    return output;
}

BString BasicInterpreter::_ExecutePrint(const char *args)
{
    BString result("ℹ️  ");

    // Simple string literal or variable print
    BString argStr(args);
    argStr.Trim();

    if (argStr.StartsWith("\"") && argStr.EndsWith("\""))
    {
        // String literal
        argStr.Remove(0, 1);
        argStr.Remove(argStr.Length() - 1, 1);
        result << argStr << "\n";
    }
    else
    {
        // Variable or expression
        float value = _EvaluateExpression(args);
        result << value << "\n";
    }

    return result;
}

BString BasicInterpreter::_ExecuteLet(const char *args)
{
    // Parse VAR = EXPR
    BString argStr(args);
    int32 eqPos = argStr.FindFirst('=');
    if (eqPos < 0)
        return "❌ LET syntax error\n";

    BString varName;
    argStr.CopyInto(varName, 0, eqPos);
    varName.Trim();

    BString exprStr;
    argStr.CopyInto(exprStr, eqPos + 1, argStr.Length() - eqPos - 1);

    float value = _EvaluateExpression(exprStr.String());
    fVariables[varName] = value;

    return ""; // Silent execution
}

BString BasicInterpreter::_ExecuteInput(const char *args)
{
    BString varName(args);
    varName.Trim();

    // In real implementation, would prompt user
    fVariables[varName] = 0;

    BString result("📝 INPUT ");
    result << varName << "\n";
    return result;
}

BString BasicInterpreter::_ExecuteGoto(const char *args)
{
    return "🚀 GOTO (stub)\n";
}

BString BasicInterpreter::_ExecuteIf(const char *args)
{
    return "✅ IF (stub)\n";
}

float BasicInterpreter::_EvaluateExpression(const char *expr)
{
    BString exprStr(expr);
    exprStr.Trim();

    // Try as number first
    char *endptr;
    float value = strtof(exprStr.String(), &endptr);
    if (*endptr == '\0')
        return value;

    // Try as variable
    if (fVariables.find(exprStr) != fVariables.end())
        return fVariables[exprStr];

    // TODO: Parse arithmetic expressions
    return 0.0;
}
