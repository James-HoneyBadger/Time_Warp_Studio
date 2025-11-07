/*
 * basic_interpreter.c - OS/2 BASIC interpreter
 * Adapted from Win2000 version for Presentation Manager
 */

#define INCL_PM
#define INCL_GPI
#include <os2.h>
#include "basic_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct
{
    char name[32];
    double value;
} Variable;

static Variable g_variables[256];
static int g_varCount = 0;
static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static HWND g_hwndCanvas = NULL;
static int g_currentLine = 0;
static int g_totalLines = 0;
static int g_lineNumbers[2048];
static int g_gosubStack[64];
static int g_gosubStackPtr = 0;

BOOL BasicInterpreter_Init(void)
{
    g_varCount = 0;
    g_running = FALSE;
    g_currentLine = 0;
    g_totalLines = 0;
    g_gosubStackPtr = 0;
    return TRUE;
}

void BasicInterpreter_Cleanup(void)
{
    g_varCount = 0;
}

static void Basic_Print(const char *text)
{
    if (g_hwndConsole)
    {
        char buf[1024];
        sprintf(buf, "%s\r\n", text);
        /* Use MLE (Multi-Line Edit) message to append text */
        WinSendMsg(g_hwndConsole, MLM_INSERT, MPFROMP(buf), NULL);
    }
}

static void Console_Write(const char *text)
{
    if (g_hwndConsole)
    {
        WinSendMsg(g_hwndConsole, MLM_INSERT, MPFROMP(text), NULL);
    }
}

/* Variable management */
static void set_var(const char *name, double value)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (stricmp(g_variables[i].name, name) == 0)
        {
            g_variables[i].value = value;
            return;
        }
    }
    if (g_varCount < 256)
    {
        strncpy(g_variables[g_varCount].name, name, 31);
        g_variables[g_varCount].name[31] = 0;
        g_variables[g_varCount].value = value;
        g_varCount++;
    }
}

static double get_var(const char *name)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (stricmp(g_variables[i].name, name) == 0)
        {
            return g_variables[i].value;
        }
    }
    return 0.0;
}

/* Simple expression evaluator */
static double eval_expr(const char *expr)
{
    char buf[256];
    strncpy(buf, expr, 255);
    buf[255] = 0;

    /* Try as variable first */
    char *p = buf;
    while (*p == ' ' || *p == '\t')
        p++;
    char *e = p;
    while (*e && *e != ' ' && *e != '\t' && *e != '+' && *e != '-' &&
           *e != '*' && *e != '/')
        e++;

    if (*e == 0)
    {
        /* Simple variable or number */
        double val = get_var(p);
        if (val != 0.0)
            return val;
        return atof(p);
    }

    /* Simple arithmetic (left-to-right, no precedence) */
    double result = 0.0;
    char op = '+';
    char *token = strtok(buf, "+-*/");

    while (token)
    {
        double val = get_var(token);
        if (val == 0.0)
            val = atof(token);

        switch (op)
        {
        case '+':
            result += val;
            break;
        case '-':
            result -= val;
            break;
        case '*':
            result *= val;
            break;
        case '/':
            if (val != 0)
                result /= val;
            break;
        }
        token = strtok(NULL, "+-*/");
    }

    return result;
}

static int find_line_index_by_number(int line_num)
{
    for (int i = 0; i < g_totalLines; i++)
    {
        if (g_lineNumbers[i] == line_num)
        {
            return i;
        }
    }
    return -1;
}

BOOL BasicInterpreter_Execute(const char *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode)
{
    g_hwndConsole = hwndConsole;
    g_hwndCanvas = hwndCanvas;
    g_running = TRUE;
    g_currentLine = 0;
    g_gosubStackPtr = 0;

    /* Parse lines and build line number map */
    char *text = strdup(code);
    char *line = text;
    g_totalLines = 0;

    for (int i = 0; i < 2048; i++)
    {
        g_lineNumbers[i] = -1;
    }

    /* Count lines and extract line numbers */
    char *p = text;
    while (*p && g_totalLines < 2048)
    {
        while (*p == ' ' || *p == '\t')
            p++;
        if (*p >= '0' && *p <= '9')
        {
            g_lineNumbers[g_totalLines] = atoi(p);
        }
        while (*p && *p != '\n')
            p++;
        if (*p == '\n')
            p++;
        g_totalLines++;
    }

    /* Execute line by line */
    line = strtok(text, "\n");
    g_currentLine = 0;

    while (line && g_running && g_currentLine < g_totalLines)
    {
        /* Skip line number if present */
        char *cmd = line;
        while (*cmd == ' ' || *cmd == '\t')
            cmd++;
        if (*cmd >= '0' && *cmd <= '9')
        {
            while (*cmd >= '0' && *cmd <= '9')
                cmd++;
            while (*cmd == ' ' || *cmd == '\t')
                cmd++;
        }

        /* Parse keyword */
        char kw[32] = {0};
        int ki = 0;
        while (*cmd && *cmd != ' ' && *cmd != '\t' && ki < 31)
        {
            kw[ki++] = *cmd++;
        }
        kw[ki] = 0;

        while (*cmd == ' ' || *cmd == '\t')
            cmd++;
        char *p = cmd;

        if (stricmp(kw, "PRINT") == 0)
        {
            Basic_Print(p);
            g_currentLine++;
        }
        else if (stricmp(kw, "LET") == 0)
        {
            char *eq = strchr(p, '=');
            if (eq)
            {
                char var[32] = {0};
                int n = (int)(eq - p);
                if (n > 31)
                    n = 31;
                strncpy(var, p, n);
                var[n] = 0;
                double v = eval_expr(eq + 1);
                set_var(var, v);
            }
            g_currentLine++;
        }
        else if (stricmp(kw, "INPUT") == 0)
        {
            char varname[32] = {0};
            strncpy(varname, p, 31);
            Console_Write("? ");
            set_var(varname, 0.0);
            g_currentLine++;
        }
        else if (stricmp(kw, "IF") == 0)
        {
            char *then = strstr(p, "THEN");
            if (then)
            {
                char cond[256];
                int n = (int)(then - p);
                if (n > 255)
                    n = 255;
                strncpy(cond, p, n);
                cond[n] = 0;
                double cv = eval_expr(cond);
                if (cv != 0)
                {
                    char *t = then + 4;
                    while (*t == ' ' || *t == '\t')
                        t++;
                    if (*t >= '0' && *t <= '9')
                    {
                        int lno = atoi(t);
                        int idx = find_line_index_by_number(lno);
                        g_currentLine = (idx >= 0 ? idx : g_currentLine + 1);
                    }
                    else
                    {
                        g_currentLine++;
                    }
                }
                else
                {
                    g_currentLine++;
                }
            }
            else
            {
                g_currentLine++;
            }
        }
        else if (stricmp(kw, "GOTO") == 0)
        {
            int lno = atoi(p);
            int idx = find_line_index_by_number(lno);
            g_currentLine = (idx >= 0 ? idx : g_currentLine + 1);
        }
        else if (stricmp(kw, "GOSUB") == 0)
        {
            int lno = atoi(p);
            int idx = find_line_index_by_number(lno);
            if (idx >= 0 && g_gosubStackPtr < 64)
            {
                g_gosubStack[g_gosubStackPtr++] = g_currentLine + 1;
                g_currentLine = idx;
            }
            else
            {
                g_currentLine++;
            }
        }
        else if (stricmp(kw, "RETURN") == 0)
        {
            if (g_gosubStackPtr > 0)
            {
                g_currentLine = g_gosubStack[--g_gosubStackPtr];
            }
            else
            {
                g_currentLine++;
            }
        }
        else if (stricmp(kw, "CLS") == 0)
        {
            /* Clear canvas via GPI */
            g_currentLine++;
        }
        else if (stricmp(kw, "END") == 0)
        {
            break;
        }
        else if (stricmp(kw, "REM") == 0)
        {
            g_currentLine++;
        }
        else
        {
            g_currentLine++;
        }

        /* Get next line for sequential execution */
        if (g_currentLine >= g_totalLines)
            break;
        line = strtok(NULL, "\n");
    }

    free(text);
    g_running = FALSE;
    return TRUE;
}

void BasicInterpreter_Stop(void)
{
    g_running = FALSE;
}

void BasicInterpreter_Reset(void)
{
    g_varCount = 0;
}

BOOL BasicInterpreter_GetVariable(const char *name, char *value, int maxLen)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (stricmp(g_variables[i].name, name) == 0)
        {
            snprintf(value, maxLen, "%g", g_variables[i].value);
            return TRUE;
        }
    }
    return FALSE;
}

BOOL BasicInterpreter_SetVariable(const char *name, const char *value)
{
    set_var(name, atof(value));
    return TRUE;
}

int BasicInterpreter_GetLineCount(void) { return g_totalLines; }
const char *BasicInterpreter_GetLine(int lineNum) { return ""; }
