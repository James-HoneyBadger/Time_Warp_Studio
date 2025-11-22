/*
 * logo_interpreter.c - OS/2 Logo interpreter
 * Adapted from Win2000 version
 */

#define INCL_PM
#define INCL_GPI
#include <os2.h>
#include "logo_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct
{
    char name[32];
    char body[4096];
} LogoProc;

static BOOL g_running = FALSE;
static HWND g_hwndCanvas = NULL;
static int g_turtleX = 400;
static int g_turtleY = 300;
static int g_turtleAngle = 0;
static BOOL g_penDown = TRUE;
static LogoProc g_procs[64];
static int g_procCount = 0;

BOOL LogoInterpreter_Init(void)
{
    g_running = FALSE;
    g_procCount = 0;
    return TRUE;
}

void LogoInterpreter_Cleanup(void)
{
    g_procCount = 0;
}

static LogoProc *find_proc(const char *name)
{
    for (int i = 0; i < g_procCount; i++)
    {
        if (stricmp(g_procs[i].name, name) == 0)
        {
            return &g_procs[i];
        }
    }
    return NULL;
}

static void store_proc(const char *name, const char *body)
{
    if (g_procCount < 64)
    {
        strncpy(g_procs[g_procCount].name, name, 31);
        g_procs[g_procCount].name[31] = 0;
        strncpy(g_procs[g_procCount].body, body, 4095);
        g_procs[g_procCount].body[4095] = 0;
        g_procCount++;
    }
}

static void Logo_Forward(int distance)
{
    if (g_hwndCanvas)
    {
        double rad = g_turtleAngle * M_PI / 180.0;
        int newX = g_turtleX + (int)(distance * sin(rad));
        int newY = g_turtleY - (int)(distance * cos(rad));

        if (g_penDown)
        {
            /* Use GPI to draw line */
            HPS hps = WinGetPS(g_hwndCanvas);
            POINTL ptl;
            ptl.x = g_turtleX;
            ptl.y = g_turtleY;
            GpiMove(hps, &ptl);
            ptl.x = newX;
            ptl.y = newY;
            GpiLine(hps, &ptl);
            WinReleasePS(hps);
        }

        g_turtleX = newX;
        g_turtleY = newY;
    }
}

static void Logo_Right(int angle)
{
    g_turtleAngle = (g_turtleAngle + angle) % 360;
}

static void Logo_Left(int angle)
{
    g_turtleAngle = (g_turtleAngle - angle + 360) % 360;
}

static void execute_logo_block(const char *code, int depth);

BOOL LogoInterpreter_Execute(const char *code, HWND hwndConsole,
                             HWND hwndCanvas, BOOL debugMode)
{
    g_hwndCanvas = hwndCanvas;
    g_running = TRUE;

    /* Two-pass: collect TO...END procedures */
    char *text = strdup(code);
    char *line = strtok(text, "\n");
    BOOL inProc = FALSE;
    char procName[32] = {0};
    char procBody[4096] = {0};

    while (line)
    {
        char *tok = line;
        while (*tok == ' ' || *tok == '\t')
            tok++;

        if (strnicmp(tok, "TO ", 3) == 0)
        {
            inProc = TRUE;
            tok += 3;
            while (*tok == ' ')
                tok++;
            sscanf(tok, "%31s", procName);
            procBody[0] = 0;
        }
        else if (inProc && stricmp(tok, "END") == 0)
        {
            store_proc(procName, procBody);
            inProc = FALSE;
        }
        else if (inProc)
        {
            strcat(procBody, line);
            strcat(procBody, "\n");
        }

        line = strtok(NULL, "\n");
    }

    /* Second pass: execute main code */
    strcpy(text, code);
    execute_logo_block(text, 0);

    free(text);
    g_running = FALSE;
    return TRUE;
}

static void execute_logo_block(const char *code, int depth)
{
    if (depth > 16)
        return;

    char *text = strdup(code);
    char *tok = strtok(text, " \n\t");

    while (tok && g_running)
    {
        if (stricmp(tok, "FORWARD") == 0 || stricmp(tok, "FD") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                Logo_Forward(atoi(tok));
        }
        else if (stricmp(tok, "BACK") == 0 || stricmp(tok, "BK") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                Logo_Forward(-atoi(tok));
        }
        else if (stricmp(tok, "RIGHT") == 0 || stricmp(tok, "RT") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                Logo_Right(atoi(tok));
        }
        else if (stricmp(tok, "LEFT") == 0 || stricmp(tok, "LT") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                Logo_Left(atoi(tok));
        }
        else if (stricmp(tok, "PENUP") == 0 || stricmp(tok, "PU") == 0)
        {
            g_penDown = FALSE;
        }
        else if (stricmp(tok, "PENDOWN") == 0 || stricmp(tok, "PD") == 0)
        {
            g_penDown = TRUE;
        }
        else if (stricmp(tok, "HOME") == 0)
        {
            g_turtleX = 400;
            g_turtleY = 300;
            g_turtleAngle = 0;
        }
        else if (stricmp(tok, "CS") == 0 || stricmp(tok, "CLEARSCREEN") == 0)
        {
            g_turtleX = 400;
            g_turtleY = 300;
            g_turtleAngle = 0;
            g_penDown = TRUE;
        }
        else
        {
            /* Check for procedure call */
            LogoProc *proc = find_proc(tok);
            if (proc)
            {
                execute_logo_block(proc->body, depth + 1);
            }
        }

        tok = strtok(NULL, " \n\t");
    }

    free(text);
}

void LogoInterpreter_Stop(void)
{
    g_running = FALSE;
}

void LogoInterpreter_Reset(void)
{
    g_procCount = 0;
    g_turtleX = 400;
    g_turtleY = 300;
    g_turtleAngle = 0;
    g_penDown = TRUE;
}
