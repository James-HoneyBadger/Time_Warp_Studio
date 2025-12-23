/*
 * forth_interpreter.c - Simple Forth Interpreter for Windows 2000
 */

#include "forth_interpreter.h"
#include "console.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static BOOL g_running = FALSE;
static int g_stack[1024];
static int g_sp = 0;

BOOL ForthInterpreter_Init(void)
{
    g_sp = 0;
    return TRUE;
}

void ForthInterpreter_Cleanup(void)
{
}

void ForthInterpreter_Stop(void)
{
    g_running = FALSE;
}

void ForthInterpreter_Reset(void)
{
    g_sp = 0;
    g_running = FALSE;
}

static void Push(int val)
{
    if (g_sp < 1024)
    {
        g_stack[g_sp++] = val;
    }
}

static int Pop(void)
{
    if (g_sp > 0)
    {
        return g_stack[--g_sp];
    }
    return 0;
}

static void ProcessToken(const TCHAR *token, HWND hwndConsole)
{
    if (strcmp(token, ".") == 0)
    {
        TCHAR buffer[64];
        sprintf(buffer, "%d ", Pop());
        Console_AppendText(hwndConsole, buffer);
    }
    else if (strcmp(token, "+") == 0)
    {
        int b = Pop();
        int a = Pop();
        Push(a + b);
    }
    else if (strcmp(token, "-") == 0)
    {
        int b = Pop();
        int a = Pop();
        Push(a - b);
    }
    else if (strcmp(token, "*") == 0)
    {
        int b = Pop();
        int a = Pop();
        Push(a * b);
    }
    else if (strcmp(token, "/") == 0)
    {
        int b = Pop();
        int a = Pop();
        if (b != 0)
            Push(a / b);
        else
            Console_AppendText(hwndConsole, "❌ Div by zero\n");
    }
    else if (strcmp(token, "CR") == 0)
    {
        Console_AppendText(hwndConsole, "\n");
    }
    else
    {
        /* Try to parse number */
        char *end;
        long val = strtol(token, &end, 10);
        if (*end == '\0')
        {
            Push((int)val);
        }
    }
}

BOOL ForthInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode)
{
    TCHAR *codeCopy;
    TCHAR *token;

    g_running = TRUE;
    g_sp = 0; /* Reset stack on run */

    if (!code)
        return FALSE;

    codeCopy = strdup(code);
    if (!codeCopy)
        return FALSE;

    Console_AppendText(hwndConsole, "🚀 Running Forth program...\n");

    token = strtok(codeCopy, " \t\r\n");
    while (token && g_running)
    {
        ProcessToken(token, hwndConsole);
        token = strtok(NULL, " \t\r\n");
    }

    Console_AppendText(hwndConsole, "\n✅ Ok\n");

    free(codeCopy);
    g_running = FALSE;
    return TRUE;
}
