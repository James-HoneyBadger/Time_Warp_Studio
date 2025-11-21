/*
 * pilot_interpreter.c - OS/2 PILOT interpreter
 * Adapted from Win2000 version
 */

#define INCL_PM
#include <os2.h>
#include "pilot_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct
{
    char name[32];
    int lineIndex;
} Label;

typedef struct
{
    char name[32];
    char value[1024];
} PilotVar;

static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static Label g_labels[256];
static int g_labelCount = 0;
static PilotVar g_vars[256];
static int g_varCount = 0;
static BOOL g_matchFlag = FALSE;
static BOOL g_conditionResult = FALSE;
static char g_lastInput[1024] = {0};

BOOL PilotInterpreter_Init(void)
{
    g_running = FALSE;
    g_labelCount = 0;
    g_varCount = 0;
    g_matchFlag = FALSE;
    return TRUE;
}

void PilotInterpreter_Cleanup(void)
{
    g_labelCount = 0;
    g_varCount = 0;
}

static void set_var(const char *name, const char *value)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (stricmp(g_vars[i].name, name) == 0)
        {
            strncpy(g_vars[i].value, value, 1023);
            g_vars[i].value[1023] = 0;
            return;
        }
    }
    if (g_varCount < 256)
    {
        strncpy(g_vars[g_varCount].name, name, 31);
        g_vars[g_varCount].name[31] = 0;
        strncpy(g_vars[g_varCount].value, value, 1023);
        g_vars[g_varCount].value[1023] = 0;
        g_varCount++;
    }
}

static const char *get_var(const char *name)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (stricmp(g_vars[i].name, name) == 0)
        {
            return g_vars[i].value;
        }
    }
    return "";
}

static void store_label(const char *name, int lineIndex)
{
    if (g_labelCount < 256)
    {
        strncpy(g_labels[g_labelCount].name, name, 31);
        g_labels[g_labelCount].name[31] = 0;
        g_labels[g_labelCount].lineIndex = lineIndex;
        g_labelCount++;
    }
}

static int find_label(const char *name)
{
    for (int i = 0; i < g_labelCount; i++)
    {
        if (stricmp(g_labels[i].name, name) == 0)
        {
            return g_labels[i].lineIndex;
        }
    }
    return -1;
}

static char *interpolate_vars(const char *text)
{
    static char buffer[2048];
    char *dst = buffer;
    const char *src = text;

    while (*src && (dst - buffer < 2047))
    {
        if (*src == '$' && isalpha(src[1]))
        {
            src++;
            char varName[32];
            int i = 0;
            while (*src && (isalnum(*src) || *src == '_') && i < 31)
            {
                varName[i++] = *src++;
            }
            varName[i] = 0;
            const char *value = get_var(varName);
            while (*value && (dst - buffer < 2047))
            {
                *dst++ = *value++;
            }
        }
        else
        {
            *dst++ = *src++;
        }
    }
    *dst = 0;
    return buffer;
}

static BOOL match_pattern(const char *pattern, const char *text)
{
    /* Simple wildcard matching with * */
    const char *p = pattern;
    const char *t = text;

    while (*p && *t)
    {
        if (*p == '*')
        {
            p++;
            if (*p == 0)
                return TRUE;
            while (*t)
            {
                if (match_pattern(p, t))
                    return TRUE;
                t++;
            }
            return FALSE;
        }
        else if (tolower(*p) == tolower(*t))
        {
            p++;
            t++;
        }
        else
        {
            return FALSE;
        }
    }
    return (*p == 0 && *t == 0);
}

static BOOL evaluate_condition(const char *cond)
{
    char *equals = strstr(cond, "=");
    if (equals)
    {
        char left[256], right[256];
        strncpy(left, cond, equals - cond);
        left[equals - cond] = 0;
        strcpy(right, equals + 1);

        /* Trim */
        char *l = left;
        while (*l == ' ')
            l++;
        char *r = right;
        while (*r == ' ')
            r++;

        return (stricmp(l, r) == 0);
    }
    return FALSE;
}

static void Console_Write(const char *text)
{
    if (g_hwndConsole)
    {
        ULONG ulLen = strlen(text);
        WinSendMsg(g_hwndConsole, MLM_INSERT, MPFROMP(text), MPFROMLONG(ulLen));
    }
}

BOOL PilotInterpreter_Execute(const char *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode)
{
    g_hwndConsole = hwndConsole;
    g_running = TRUE;
    g_matchFlag = FALSE;

    char **lines = (char **)malloc(4096 * sizeof(char *));
    int lineCount = 0;

    /* Split into lines */
    char *text = strdup(code);
    char *line = strtok(text, "\n");
    while (line && lineCount < 4096)
    {
        lines[lineCount++] = strdup(line);
        line = strtok(NULL, "\n");
    }
    free(text);

    /* First pass: collect labels */
    for (int i = 0; i < lineCount; i++)
    {
        char *ln = lines[i];
        while (*ln == ' ' || *ln == '\t')
            ln++;

        if (*ln == '*')
        {
            ln++;
            char labelName[32];
            sscanf(ln, "%31s", labelName);
            store_label(labelName, i);
        }
    }

    /* Second pass: execute */
    int pc = 0;
    while (pc < lineCount && g_running)
    {
        char *ln = lines[pc];
        while (*ln == ' ' || *ln == '\t')
            ln++;

        if (*ln == 0 || *ln == '*')
        {
            pc++;
            continue;
        }

        char cmd = toupper(*ln);
        char *arg = ln + 1;
        if (*arg == ':')
            arg++;
        while (*arg == ' ')
            arg++;

        switch (cmd)
        {
        case 'T':
        { /* Type */
            char *text = interpolate_vars(arg);
            Console_Write(text);
            Console_Write("\n");
            break;
        }

        case 'A':
        { /* Accept */
            set_var(arg, g_lastInput);
            break;
        }

        case 'M':
        { /* Match */
            g_matchFlag = match_pattern(arg, g_lastInput);
            break;
        }

        case 'Y':
        { /* Yes (if match) */
            if (g_matchFlag)
            {
                char *text = interpolate_vars(arg);
                Console_Write(text);
                Console_Write("\n");
            }
            break;
        }

        case 'N':
        { /* No (if not match) */
            if (!g_matchFlag)
            {
                char *text = interpolate_vars(arg);
                Console_Write(text);
                Console_Write("\n");
            }
            break;
        }

        case 'C':
        { /* Compute/Condition */
            g_conditionResult = evaluate_condition(arg);
            break;
        }

        case 'U':
        { /* Use (if condition true) */
            if (g_conditionResult)
            {
                int target = find_label(arg);
                if (target >= 0)
                    pc = target - 1;
            }
            break;
        }

        case 'J':
        { /* Jump */
            int target = find_label(arg);
            if (target >= 0)
                pc = target - 1;
            break;
        }

        case 'E':
        { /* End */
            g_running = FALSE;
            break;
        }

        case 'R':
        { /* Remark */
            /* Comment, do nothing */
            break;
        }
        }

        pc++;
    }

    /* Cleanup */
    for (int i = 0; i < lineCount; i++)
    {
        free(lines[i]);
    }
    free(lines);

    g_running = FALSE;
    return TRUE;
}

void PilotInterpreter_Stop(void)
{
    g_running = FALSE;
}

void PilotInterpreter_Reset(void)
{
    g_labelCount = 0;
    g_varCount = 0;
    g_matchFlag = FALSE;
    g_conditionResult = FALSE;
    g_lastInput[0] = 0;
}
