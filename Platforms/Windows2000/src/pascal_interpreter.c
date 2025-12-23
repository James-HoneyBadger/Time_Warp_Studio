/*
 * pascal_interpreter.c - Simple Pascal Interpreter for Windows 2000
 */

#include "pascal_interpreter.h"
#include "console.h"
#include <stdio.h>
#include <string.h>

static BOOL g_running = FALSE;

BOOL PascalInterpreter_Init(void)
{
    return TRUE;
}

void PascalInterpreter_Cleanup(void)
{
}

void PascalInterpreter_Stop(void)
{
    g_running = FALSE;
}

void PascalInterpreter_Reset(void)
{
    g_running = FALSE;
}

/* Simple string parser helper */
static void ParseAndExecute(const TCHAR *line, HWND hwndConsole)
{
    /* Check for writeln */
    if (strstr(line, "writeln") || strstr(line, "WRITELN"))
    {
        const TCHAR *start = strchr(line, '\'');
        if (start)
        {
            const TCHAR *end = strchr(start + 1, '\'');
            if (end)
            {
                TCHAR buffer[1024];
                int len = (int)(end - start) - 1;
                if (len > 0 && len < 1024)
                {
                    strncpy(buffer, start + 1, len);
                    buffer[len] = '\0';
                    strcat(buffer, "\n");
                    Console_AppendText(hwndConsole, buffer);
                }
            }
        }
    }
}

BOOL PascalInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                               HWND hwndCanvas, BOOL debugMode)
{
    TCHAR *codeCopy;
    TCHAR *line;
    TCHAR *context = NULL;

    g_running = TRUE;

    if (!code)
        return FALSE;

    /* Make a copy to tokenize */
    codeCopy = strdup(code);
    if (!codeCopy)
        return FALSE;

    Console_AppendText(hwndConsole, "🚀 Running Pascal program...\n");

    line = strtok(codeCopy, "\r\n");
    while (line && g_running)
    {
        ParseAndExecute(line, hwndConsole);
        line = strtok(NULL, "\r\n");
    }

    free(codeCopy);
    g_running = FALSE;
    return TRUE;
}
