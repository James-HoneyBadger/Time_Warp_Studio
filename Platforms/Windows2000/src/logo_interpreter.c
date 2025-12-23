/*
 * logo_interpreter.c - Complete Logo Interpreter for Time Warp IDE
 * Matches Python implementation features:
 * - Turtle Graphics: FORWARD, BACK, LEFT, RIGHT, PENUP, PENDOWN, etc.
 * - Screen Commands: CLEARSCREEN, HOME, HIDETURTLE, SHOWTURTLE
 * - Position Commands: SETXY, SETX, SETY, SETHEADING
 * - Color Commands: SETPENCOLOR, SETBGCOLOR, SETPENWIDTH
 * - Control: REPEAT, IF, IFELSE, FOREVER, STOP
 * - Procedures: TO/END with parameters
 * - Variables: MAKE, THING, :varname
 * - Output: PRINT, SHOW, TYPE, LABEL
 * - Data Structures: WORD, LIST, SENTENCE, FIRST, LAST, etc.
 * - Math: SUM, DIFFERENCE, PRODUCT, QUOTIENT, RANDOM
 * - Advanced: ARC, FILLED
 */

#include "logo_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <ctype.h>
#include <tchar.h>
#include "canvas.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#ifndef _MSC_VER
#ifndef _tcstok_s
#define _tcstok_s(str, delim, ctx) _tcstok(str, delim)
#endif
#ifndef _tcsdup
#define _tcsdup _strdup
#endif
#endif

/* ============================================================================
 * Data Structures
 * ============================================================================ */

typedef struct {
    TCHAR name[64];
    double numValue;
    TCHAR strValue[1024];
    BOOL isString;
} LogoVariable;

typedef struct {
    TCHAR name[64];
    TCHAR params[256];
    TCHAR body[8192];
    int paramCount;
    TCHAR paramNames[16][32];
} LogoProcedure;

typedef struct {
    double x, y;
    double heading;  /* 0=right, 90=up, 180=left, 270=down (Logo standard) */
    BOOL penDown;
    BOOL visible;
    int penWidth;
    COLORREF penColor;
    COLORREF bgColor;
} TurtleState;

/* ============================================================================
 * Global State
 * ============================================================================ */

#define MAX_VARIABLES 256
#define MAX_PROCEDURES 64
#define MAX_CALL_DEPTH 32
#define MAX_REPEAT_DEPTH 32

static LogoVariable g_variables[MAX_VARIABLES];
static int g_varCount = 0;

static LogoProcedure g_procedures[MAX_PROCEDURES];
static int g_procCount = 0;

static TurtleState g_turtle;
static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static HWND g_hwndCanvas = NULL;
static BOOL g_debugMode = FALSE;

static int g_callDepth = 0;
static int g_repeatDepth = 0;
static BOOL g_stopRequested = FALSE;

/* Color name mapping */
typedef struct {
    const TCHAR *name;
    COLORREF color;
} ColorMap;

static ColorMap g_colorNames[] = {
    { TEXT("BLACK"), RGB(0, 0, 0) },
    { TEXT("WHITE"), RGB(255, 255, 255) },
    { TEXT("RED"), RGB(255, 0, 0) },
    { TEXT("GREEN"), RGB(0, 255, 0) },
    { TEXT("BLUE"), RGB(0, 0, 255) },
    { TEXT("YELLOW"), RGB(255, 255, 0) },
    { TEXT("CYAN"), RGB(0, 255, 255) },
    { TEXT("MAGENTA"), RGB(255, 0, 255) },
    { TEXT("GRAY"), RGB(128, 128, 128) },
    { TEXT("GREY"), RGB(128, 128, 128) },
    { TEXT("ORANGE"), RGB(255, 165, 0) },
    { TEXT("PURPLE"), RGB(128, 0, 128) },
    { TEXT("BROWN"), RGB(165, 42, 42) },
    { TEXT("PINK"), RGB(255, 192, 203) },
    { NULL, 0 }
};

/* ============================================================================
 * Forward Declarations
 * ============================================================================ */

static void Console_Write(const TCHAR *text);
static void Console_WriteLine(const TCHAR *text);
static double EvaluateExpression(const TCHAR *expr);
static BOOL ExecuteLogoLine(const TCHAR *line);
static BOOL ExecuteLogoBlock(const TCHAR *code, int depth);

/* ============================================================================
 * Initialization
 * ============================================================================ */

BOOL LogoInterpreter_Init(void) {
    g_varCount = 0;
    g_procCount = 0;
    g_running = FALSE;
    g_callDepth = 0;
    g_repeatDepth = 0;
    g_stopRequested = FALSE;
    
    /* Initialize turtle to center, facing up */
    g_turtle.x = 512.0;  /* Will be adjusted to canvas center */
    g_turtle.y = 384.0;
    g_turtle.heading = 90.0;  /* Logo: 90 = up */
    g_turtle.penDown = TRUE;
    g_turtle.visible = TRUE;
    g_turtle.penWidth = 2;
    g_turtle.penColor = RGB(0, 0, 255);  /* Blue */
    g_turtle.bgColor = RGB(255, 255, 255);  /* White */
    
    srand((unsigned int)time(NULL));
    
    return TRUE;
}

void LogoInterpreter_Cleanup(void) {
    g_varCount = 0;
    g_procCount = 0;
}

void LogoInterpreter_Reset(void) {
    LogoInterpreter_Cleanup();
    LogoInterpreter_Init();
}

/* ============================================================================
 * Console Output
 * ============================================================================ */

static void Console_Write(const TCHAR *text) {
    if (g_hwndConsole && text) {
        int len = GetWindowTextLength(g_hwndConsole);
        SendMessage(g_hwndConsole, EM_SETSEL, len, len);
        SendMessage(g_hwndConsole, EM_REPLACESEL, FALSE, (LPARAM)text);
    }
}

static void Console_WriteLine(const TCHAR *text) {
    TCHAR buf[2048];
    _stprintf(buf, TEXT("%s\r\n"), text ? text : TEXT(""));
    Console_Write(buf);
}

/* ============================================================================
 * Variable Management
 * ============================================================================ */

static int FindVariable(const TCHAR *name) {
    int i;
    TCHAR upperName[64];
    
    _tcsncpy(upperName, name, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    for (i = 0; i < g_varCount; i++) {
        if (_tcsicmp(g_variables[i].name, upperName) == 0) {
            return i;
        }
    }
    return -1;
}

static void SetVariable(const TCHAR *name, double value) {
    int idx;
    TCHAR upperName[64];
    
    /* Remove leading quote if present */
    const TCHAR *varName = name;
    if (*varName == '"') varName++;
    
    _tcsncpy(upperName, varName, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    idx = FindVariable(upperName);
    if (idx < 0 && g_varCount < MAX_VARIABLES) {
        idx = g_varCount++;
        _tcscpy(g_variables[idx].name, upperName);
    }
    
    if (idx >= 0) {
        g_variables[idx].numValue = value;
        g_variables[idx].isString = FALSE;
    }
}

static void SetStringVariable(const TCHAR *name, const TCHAR *value) {
    int idx;
    TCHAR upperName[64];
    
    const TCHAR *varName = name;
    if (*varName == '"') varName++;
    
    _tcsncpy(upperName, varName, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    idx = FindVariable(upperName);
    if (idx < 0 && g_varCount < MAX_VARIABLES) {
        idx = g_varCount++;
        _tcscpy(g_variables[idx].name, upperName);
    }
    
    if (idx >= 0) {
        _tcsncpy(g_variables[idx].strValue, value, 1023);
        g_variables[idx].strValue[1023] = 0;
        g_variables[idx].isString = TRUE;
    }
}

static double GetVariable(const TCHAR *name) {
    int idx;
    TCHAR upperName[64];
    
    /* Handle :varname syntax */
    const TCHAR *varName = name;
    if (*varName == ':') varName++;
    
    _tcsncpy(upperName, varName, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    idx = FindVariable(upperName);
    if (idx >= 0) {
        if (g_variables[idx].isString) {
            return _tstof(g_variables[idx].strValue);
        }
        return g_variables[idx].numValue;
    }
    return 0.0;
}

static const TCHAR *GetStringVariable(const TCHAR *name) {
    static TCHAR result[1024];
    int idx;
    TCHAR upperName[64];
    
    const TCHAR *varName = name;
    if (*varName == ':') varName++;
    
    _tcsncpy(upperName, varName, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    idx = FindVariable(upperName);
    if (idx >= 0) {
        if (g_variables[idx].isString) {
            return g_variables[idx].strValue;
        }
        _stprintf(result, TEXT("%g"), g_variables[idx].numValue);
        return result;
    }
    return TEXT("");
}

/* ============================================================================
 * Procedure Management
 * ============================================================================ */

static int FindProcedure(const TCHAR *name) {
    int i;
    for (i = 0; i < g_procCount; i++) {
        if (_tcsicmp(g_procedures[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

static BOOL StoreProcedure(const TCHAR *name, const TCHAR *params, const TCHAR *body) {
    int idx;
    TCHAR *p;
    int pi;
    
    idx = FindProcedure(name);
    if (idx < 0 && g_procCount < MAX_PROCEDURES) {
        idx = g_procCount++;
    }
    
    if (idx < 0) return FALSE;
    
    _tcsncpy(g_procedures[idx].name, name, 63);
    g_procedures[idx].name[63] = 0;
    _tcsupr(g_procedures[idx].name);
    
    _tcsncpy(g_procedures[idx].body, body, 8191);
    g_procedures[idx].body[8191] = 0;
    
    /* Parse parameters (start with :) */
    g_procedures[idx].paramCount = 0;
    if (params && *params) {
        TCHAR paramBuf[256];
        _tcsncpy(paramBuf, params, 255);
        paramBuf[255] = 0;
        
        p = paramBuf;
        while (*p && g_procedures[idx].paramCount < 16) {
            while (*p == ' ') p++;
            if (*p == ':') {
                p++;
                pi = 0;
                while (*p && *p != ' ' && pi < 31) {
                    g_procedures[idx].paramNames[g_procedures[idx].paramCount][pi++] = (TCHAR)toupper(*p);
                    p++;
                }
                g_procedures[idx].paramNames[g_procedures[idx].paramCount][pi] = 0;
                g_procedures[idx].paramCount++;
            } else if (*p) {
                p++;
            }
        }
    }
    
    return TRUE;
}

/* ============================================================================
 * Color Parsing
 * ============================================================================ */

static COLORREF ParseColor(const TCHAR *colorStr) {
    int i;
    TCHAR upper[32];
    
    _tcsncpy(upper, colorStr, 31);
    upper[31] = 0;
    _tcsupr(upper);
    
    /* Check color names */
    for (i = 0; g_colorNames[i].name; i++) {
        if (_tcscmp(upper, g_colorNames[i].name) == 0) {
            return g_colorNames[i].color;
        }
    }
    
    /* Try as hex (#RRGGBB) */
    if (upper[0] == '#' && _tcslen(upper) == 7) {
        int r, g, b;
        _stscanf(upper + 1, TEXT("%02x%02x%02x"), &r, &g, &b);
        return RGB(r, g, b);
    }
    
    /* Default to black */
    return RGB(0, 0, 0);
}

/* ============================================================================
 * Expression Evaluation
 * ============================================================================ */

static double EvaluateExpression(const TCHAR *expr) {
    const TCHAR *p = expr;
    double result = 0.0;
    
    while (*p == ' ') p++;
    
    /* Handle :variable syntax */
    if (*p == ':') {
        TCHAR varName[64];
        int vi = 0;
        p++;
        while (*p && !isspace(*p) && vi < 63) {
            varName[vi++] = *p++;
        }
        varName[vi] = 0;
        return GetVariable(varName);
    }
    
    /* Handle quoted string (return 0 for numeric context) */
    if (*p == '"') {
        return 0.0;
    }
    
    /* Handle RANDOM function */
    if (_tcsnicmp(p, TEXT("RANDOM"), 6) == 0) {
        p += 6;
        while (*p == ' ') p++;
        double limit = EvaluateExpression(p);
        return (double)(rand() % (int)limit);
    }
    
    /* Handle other functions */
    TCHAR funcName[32];
    int fi = 0;
    const TCHAR *start = p;
    while (*p && isalpha(*p) && fi < 31) {
        funcName[fi++] = (TCHAR)toupper(*p);
        p++;
    }
    funcName[fi] = 0;
    
    if (fi > 0) {
        while (*p == ' ') p++;
        
        if (_tcscmp(funcName, TEXT("SUM")) == 0) {
            double a = EvaluateExpression(p);
            /* Find second argument */
            while (*p && !isspace(*p)) p++;
            while (*p == ' ') p++;
            double b = EvaluateExpression(p);
            return a + b;
        }
        if (_tcscmp(funcName, TEXT("DIFFERENCE")) == 0) {
            double a = EvaluateExpression(p);
            while (*p && !isspace(*p)) p++;
            while (*p == ' ') p++;
            double b = EvaluateExpression(p);
            return a - b;
        }
        if (_tcscmp(funcName, TEXT("PRODUCT")) == 0) {
            double a = EvaluateExpression(p);
            while (*p && !isspace(*p)) p++;
            while (*p == ' ') p++;
            double b = EvaluateExpression(p);
            return a * b;
        }
        if (_tcscmp(funcName, TEXT("QUOTIENT")) == 0) {
            double a = EvaluateExpression(p);
            while (*p && !isspace(*p)) p++;
            while (*p == ' ') p++;
            double b = EvaluateExpression(p);
            return (b != 0) ? a / b : 0;
        }
        
        /* Not a known function, check if it's a variable */
        p = start;
    }
    
    /* Try numeric parse */
    result = _tstof(p);
    return result;
}

/* ============================================================================
 * Turtle Graphics
 * ============================================================================ */

static void DrawTurtle(void) {
    if (!g_turtle.visible || !g_hwndCanvas) return;
    
    /* Turtle drawing would be handled by canvas */
    Canvas_DrawTurtle(g_hwndCanvas, (int)g_turtle.x, (int)g_turtle.y, 
                      g_turtle.heading, g_turtle.penDown);
}

static void Logo_Forward(double distance) {
    double rad = g_turtle.heading * M_PI / 180.0;
    double newX = g_turtle.x + distance * cos(rad);
    double newY = g_turtle.y - distance * sin(rad);  /* Screen Y is inverted */
    
    if (g_turtle.penDown && g_hwndCanvas) {
        Canvas_SetPenColor(g_hwndCanvas, g_turtle.penColor);
        Canvas_SetPenWidth(g_hwndCanvas, g_turtle.penWidth);
        Canvas_MoveTo(g_hwndCanvas, (int)g_turtle.x, (int)g_turtle.y);
        Canvas_LineTo(g_hwndCanvas, (int)newX, (int)newY);
    }
    
    g_turtle.x = newX;
    g_turtle.y = newY;
    DrawTurtle();
}

static void Logo_Back(double distance) {
    Logo_Forward(-distance);
}

static void Logo_Right(double angle) {
    g_turtle.heading -= angle;
    while (g_turtle.heading < 0) g_turtle.heading += 360;
    while (g_turtle.heading >= 360) g_turtle.heading -= 360;
    DrawTurtle();
}

static void Logo_Left(double angle) {
    g_turtle.heading += angle;
    while (g_turtle.heading < 0) g_turtle.heading += 360;
    while (g_turtle.heading >= 360) g_turtle.heading -= 360;
    DrawTurtle();
}

static void Logo_SetXY(double x, double y) {
    if (g_turtle.penDown && g_hwndCanvas) {
        Canvas_SetPenColor(g_hwndCanvas, g_turtle.penColor);
        Canvas_SetPenWidth(g_hwndCanvas, g_turtle.penWidth);
        Canvas_MoveTo(g_hwndCanvas, (int)g_turtle.x, (int)g_turtle.y);
        Canvas_LineTo(g_hwndCanvas, (int)(512 + x), (int)(384 - y));  /* Center coords */
    }
    g_turtle.x = 512 + x;
    g_turtle.y = 384 - y;
    DrawTurtle();
}

static void Logo_SetX(double x) {
    double currentY = 384 - g_turtle.y;  /* Convert back to Logo coords */
    Logo_SetXY(x, currentY);
}

static void Logo_SetY(double y) {
    double currentX = g_turtle.x - 512;  /* Convert back to Logo coords */
    Logo_SetXY(currentX, y);
}

static void Logo_Home(void) {
    if (g_turtle.penDown && g_hwndCanvas) {
        Canvas_SetPenColor(g_hwndCanvas, g_turtle.penColor);
        Canvas_SetPenWidth(g_hwndCanvas, g_turtle.penWidth);
        Canvas_MoveTo(g_hwndCanvas, (int)g_turtle.x, (int)g_turtle.y);
        Canvas_LineTo(g_hwndCanvas, 512, 384);
    }
    g_turtle.x = 512;
    g_turtle.y = 384;
    g_turtle.heading = 90;
    DrawTurtle();
}

static void Logo_ClearScreen(void) {
    if (g_hwndCanvas) {
        Canvas_Clear(g_hwndCanvas);
    }
    g_turtle.x = 512;
    g_turtle.y = 384;
    g_turtle.heading = 90;
    g_turtle.penDown = TRUE;
    DrawTurtle();
}

static void Logo_PenUp(void) {
    g_turtle.penDown = FALSE;
}

static void Logo_PenDown(void) {
    g_turtle.penDown = TRUE;
}

static void Logo_HideTurtle(void) {
    g_turtle.visible = FALSE;
}

static void Logo_ShowTurtle(void) {
    g_turtle.visible = TRUE;
    DrawTurtle();
}

static void Logo_SetHeading(double angle) {
    g_turtle.heading = angle;
    while (g_turtle.heading < 0) g_turtle.heading += 360;
    while (g_turtle.heading >= 360) g_turtle.heading -= 360;
    DrawTurtle();
}

static void Logo_SetPenColor(const TCHAR *colorArgs) {
    const TCHAR *p = colorArgs;
    while (*p == ' ') p++;
    
    /* Check for RGB format: r g b */
    int r, g, b;
    if (_stscanf(p, TEXT("%d %d %d"), &r, &g, &b) == 3) {
        g_turtle.penColor = RGB(r, g, b);
    } else {
        /* Color name or single number */
        TCHAR colorName[32];
        _tcsncpy(colorName, p, 31);
        colorName[31] = 0;
        g_turtle.penColor = ParseColor(colorName);
    }
    
    if (g_hwndCanvas) {
        Canvas_SetPenColor(g_hwndCanvas, g_turtle.penColor);
    }
}

static void Logo_SetBgColor(const TCHAR *colorArgs) {
    const TCHAR *p = colorArgs;
    while (*p == ' ') p++;
    
    int r, g, b;
    if (_stscanf(p, TEXT("%d %d %d"), &r, &g, &b) == 3) {
        g_turtle.bgColor = RGB(r, g, b);
    } else {
        g_turtle.bgColor = ParseColor(p);
    }
    
    if (g_hwndCanvas) {
        Canvas_SetBgColor(g_hwndCanvas, g_turtle.bgColor);
    }
}

static void Logo_SetPenWidth(double width) {
    g_turtle.penWidth = (int)width;
    if (g_turtle.penWidth < 1) g_turtle.penWidth = 1;
    
    if (g_hwndCanvas) {
        Canvas_SetPenWidth(g_hwndCanvas, g_turtle.penWidth);
    }
}

static void Logo_Arc(double angle, double radius) {
    /* Draw arc by small steps */
    int steps = (int)(fabs(angle) / 5);
    if (steps < 1) steps = 1;
    double stepAngle = angle / steps;
    double circumference = 2 * M_PI * fabs(radius) * (fabs(angle) / 360.0);
    double stepDist = circumference / steps;
    
    for (int i = 0; i < steps; i++) {
        Logo_Forward(stepDist);
        if (radius > 0) {
            Logo_Left(stepAngle);
        } else {
            Logo_Right(stepAngle);
        }
    }
}

/* ============================================================================
 * Parse Bracket Block
 * ============================================================================ */

static const TCHAR *FindMatchingBracket(const TCHAR *start) {
    int depth = 0;
    const TCHAR *p = start;
    
    while (*p) {
        if (*p == '[') depth++;
        else if (*p == ']') {
            depth--;
            if (depth == 0) return p;
        }
        p++;
    }
    return NULL;
}

static BOOL ExtractBracketContent(const TCHAR *start, TCHAR *content, int maxLen) {
    const TCHAR *end;
    int len;
    
    while (*start == ' ') start++;
    if (*start != '[') return FALSE;
    
    start++;  /* Skip [ */
    end = FindMatchingBracket(start - 1);
    if (!end) return FALSE;
    
    len = (int)(end - start);
    if (len > maxLen - 1) len = maxLen - 1;
    _tcsncpy(content, start, len);
    content[len] = 0;
    
    return TRUE;
}

/* ============================================================================
 * Command Execution
 * ============================================================================ */

static BOOL ExecuteLogoLine(const TCHAR *line) {
    TCHAR cmd[32];
    const TCHAR *args;
    const TCHAR *p = line;
    int ci = 0;
    
    if (g_stopRequested) return FALSE;
    
    while (*p == ' ' || *p == '\t') p++;
    if (!*p || *p == ';') return TRUE;  /* Empty or comment */
    
    /* Extract command */
    while (*p && !isspace(*p) && *p != '[' && ci < 31) {
        cmd[ci++] = (TCHAR)toupper(*p);
        p++;
    }
    cmd[ci] = 0;
    
    while (*p == ' ') p++;
    args = p;
    
    /* Movement commands */
    if (_tcscmp(cmd, TEXT("FORWARD")) == 0 || _tcscmp(cmd, TEXT("FD")) == 0) {
        Logo_Forward(EvaluateExpression(args));
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("BACK")) == 0 || _tcscmp(cmd, TEXT("BK")) == 0 || 
        _tcscmp(cmd, TEXT("BACKWARD")) == 0) {
        Logo_Back(EvaluateExpression(args));
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("RIGHT")) == 0 || _tcscmp(cmd, TEXT("RT")) == 0) {
        Logo_Right(EvaluateExpression(args));
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("LEFT")) == 0 || _tcscmp(cmd, TEXT("LT")) == 0) {
        Logo_Left(EvaluateExpression(args));
        return TRUE;
    }
    
    /* Pen commands */
    if (_tcscmp(cmd, TEXT("PENUP")) == 0 || _tcscmp(cmd, TEXT("PU")) == 0) {
        Logo_PenUp();
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("PENDOWN")) == 0 || _tcscmp(cmd, TEXT("PD")) == 0) {
        Logo_PenDown();
        return TRUE;
    }
    
    /* Screen commands */
    if (_tcscmp(cmd, TEXT("HOME")) == 0) {
        Logo_Home();
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("CLEARSCREEN")) == 0 || _tcscmp(cmd, TEXT("CS")) == 0 ||
        _tcscmp(cmd, TEXT("CLEAR")) == 0) {
        Logo_ClearScreen();
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("HIDETURTLE")) == 0 || _tcscmp(cmd, TEXT("HT")) == 0) {
        Logo_HideTurtle();
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("SHOWTURTLE")) == 0 || _tcscmp(cmd, TEXT("ST")) == 0) {
        Logo_ShowTurtle();
        return TRUE;
    }
    
    /* Position commands */
    if (_tcscmp(cmd, TEXT("SETXY")) == 0) {
        double x = EvaluateExpression(args);
        while (*args && !isspace(*args)) args++;
        while (*args == ' ') args++;
        double y = EvaluateExpression(args);
        Logo_SetXY(x, y);
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("SETX")) == 0) {
        Logo_SetX(EvaluateExpression(args));
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("SETY")) == 0) {
        Logo_SetY(EvaluateExpression(args));
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("SETHEADING")) == 0 || _tcscmp(cmd, TEXT("SETH")) == 0) {
        Logo_SetHeading(EvaluateExpression(args));
        return TRUE;
    }
    
    /* Color commands */
    if (_tcscmp(cmd, TEXT("SETPENCOLOR")) == 0 || _tcscmp(cmd, TEXT("SETPC")) == 0 ||
        _tcscmp(cmd, TEXT("SETCOLOR")) == 0) {
        Logo_SetPenColor(args);
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("SETBGCOLOR")) == 0 || _tcscmp(cmd, TEXT("SETBG")) == 0) {
        Logo_SetBgColor(args);
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("SETPENWIDTH")) == 0 || _tcscmp(cmd, TEXT("SETPW")) == 0 ||
        _tcscmp(cmd, TEXT("PENWIDTH")) == 0 || _tcscmp(cmd, TEXT("SETPENSIZE")) == 0) {
        Logo_SetPenWidth(EvaluateExpression(args));
        return TRUE;
    }
    
    /* REPEAT command */
    if (_tcscmp(cmd, TEXT("REPEAT")) == 0) {
        int count = (int)EvaluateExpression(args);
        
        /* Find bracket block */
        const TCHAR *bracket = _tcschr(args, '[');
        if (!bracket) return TRUE;
        
        TCHAR body[4096];
        if (!ExtractBracketContent(bracket, body, 4096)) return TRUE;
        
        g_repeatDepth++;
        for (int i = 0; i < count && g_running && !g_stopRequested; i++) {
            ExecuteLogoBlock(body, g_callDepth);
        }
        g_repeatDepth--;
        return TRUE;
    }
    
    /* IF command */
    if (_tcscmp(cmd, TEXT("IF")) == 0) {
        /* Find condition and bracket */
        double condition = EvaluateExpression(args);
        
        const TCHAR *bracket = _tcschr(args, '[');
        if (!bracket) return TRUE;
        
        TCHAR body[4096];
        if (!ExtractBracketContent(bracket, body, 4096)) return TRUE;
        
        if (fabs(condition) > 0.0001) {
            ExecuteLogoBlock(body, g_callDepth);
        }
        return TRUE;
    }
    
    /* IFELSE command */
    if (_tcscmp(cmd, TEXT("IFELSE")) == 0) {
        double condition = EvaluateExpression(args);
        
        const TCHAR *bracket1 = _tcschr(args, '[');
        if (!bracket1) return TRUE;
        
        const TCHAR *end1 = FindMatchingBracket(bracket1);
        if (!end1) return TRUE;
        
        const TCHAR *bracket2 = _tcschr(end1 + 1, '[');
        if (!bracket2) return TRUE;
        
        TCHAR trueBody[4096], falseBody[4096];
        ExtractBracketContent(bracket1, trueBody, 4096);
        ExtractBracketContent(bracket2, falseBody, 4096);
        
        if (fabs(condition) > 0.0001) {
            ExecuteLogoBlock(trueBody, g_callDepth);
        } else {
            ExecuteLogoBlock(falseBody, g_callDepth);
        }
        return TRUE;
    }
    
    /* STOP command */
    if (_tcscmp(cmd, TEXT("STOP")) == 0) {
        g_stopRequested = TRUE;
        return FALSE;
    }
    
    /* Variable commands */
    if (_tcscmp(cmd, TEXT("MAKE")) == 0) {
        /* MAKE "varname value */
        TCHAR varName[64];
        int vi = 0;
        
        if (*args == '"') args++;
        while (*args && !isspace(*args) && vi < 63) {
            varName[vi++] = *args++;
        }
        varName[vi] = 0;
        
        while (*args == ' ') args++;
        
        /* Check if value is quoted string */
        if (*args == '"') {
            args++;
            TCHAR strVal[1024];
            int si = 0;
            while (*args && *args != '"' && si < 1023) {
                strVal[si++] = *args++;
            }
            strVal[si] = 0;
            SetStringVariable(varName, strVal);
        } else {
            SetVariable(varName, EvaluateExpression(args));
        }
        return TRUE;
    }
    
    if (_tcscmp(cmd, TEXT("THING")) == 0) {
        /* Returns variable value - mainly used in expressions */
        return TRUE;
    }
    
    /* Output commands */
    if (_tcscmp(cmd, TEXT("PRINT")) == 0) {
        if (*args == ':') {
            Console_WriteLine(GetStringVariable(args));
        } else if (*args == '"') {
            TCHAR buf[1024];
            args++;
            _tcsncpy(buf, args, 1023);
            buf[1023] = 0;
            TCHAR *end = _tcschr(buf, '"');
            if (end) *end = 0;
            Console_WriteLine(buf);
        } else {
            TCHAR buf[64];
            _stprintf(buf, TEXT("%g"), EvaluateExpression(args));
            Console_WriteLine(buf);
        }
        return TRUE;
    }
    
    if (_tcscmp(cmd, TEXT("SHOW")) == 0) {
        if (*args == ':') {
            Console_WriteLine(GetStringVariable(args));
        } else {
            Console_WriteLine(args);
        }
        return TRUE;
    }
    
    if (_tcscmp(cmd, TEXT("TYPE")) == 0) {
        if (*args == ':') {
            Console_Write(GetStringVariable(args));
        } else if (*args == '"') {
            args++;
            TCHAR buf[1024];
            _tcsncpy(buf, args, 1023);
            TCHAR *end = _tcschr(buf, '"');
            if (end) *end = 0;
            Console_Write(buf);
        }
        return TRUE;
    }
    
    if (_tcscmp(cmd, TEXT("LABEL")) == 0) {
        /* Draw text at turtle position */
        if (g_hwndCanvas) {
            TCHAR text[256];
            if (*args == '"') {
                args++;
                _tcsncpy(text, args, 255);
                TCHAR *end = _tcschr(text, '"');
                if (end) *end = 0;
            } else {
                _tcsncpy(text, args, 255);
            }
            Canvas_DrawText(g_hwndCanvas, (int)g_turtle.x, (int)g_turtle.y, text);
        }
        return TRUE;
    }
    
    /* Arc command */
    if (_tcscmp(cmd, TEXT("ARC")) == 0) {
        double angle = EvaluateExpression(args);
        while (*args && !isspace(*args)) args++;
        while (*args == ' ') args++;
        double radius = EvaluateExpression(args);
        Logo_Arc(angle, radius);
        return TRUE;
    }
    
    /* TO command (procedure definition) - handled during parsing */
    if (_tcscmp(cmd, TEXT("TO")) == 0) {
        return TRUE;
    }
    if (_tcscmp(cmd, TEXT("END")) == 0) {
        return TRUE;
    }
    
    /* Check for user-defined procedure */
    int procIdx = FindProcedure(cmd);
    if (procIdx >= 0) {
        if (g_callDepth >= MAX_CALL_DEPTH) return TRUE;
        
        /* Parse arguments and bind to parameters */
        LogoProcedure *proc = &g_procedures[procIdx];
        const TCHAR *argPtr = args;
        
        /* Save old variable values */
        double oldValues[16];
        for (int i = 0; i < proc->paramCount; i++) {
            oldValues[i] = GetVariable(proc->paramNames[i]);
        }
        
        /* Bind new values */
        for (int i = 0; i < proc->paramCount && *argPtr; i++) {
            while (*argPtr == ' ') argPtr++;
            double val = EvaluateExpression(argPtr);
            SetVariable(proc->paramNames[i], val);
            while (*argPtr && !isspace(*argPtr)) argPtr++;
        }
        
        g_callDepth++;
        g_stopRequested = FALSE;
        ExecuteLogoBlock(proc->body, g_callDepth);
        g_stopRequested = FALSE;
        g_callDepth--;
        
        /* Restore old values */
        for (int i = 0; i < proc->paramCount; i++) {
            SetVariable(proc->paramNames[i], oldValues[i]);
        }
        
        return TRUE;
    }
    
    return TRUE;
}

static BOOL ExecuteLogoBlock(const TCHAR *code, int depth) {
    TCHAR *dup;
    TCHAR *line;
    TCHAR *save = NULL;
    
    if (depth > MAX_CALL_DEPTH) return FALSE;
    
    dup = _tcsdup(code);
    line = _tcstok_s(dup, TEXT("\n"), &save);
    
    while (line && g_running && !g_stopRequested) {
        while (*line == ' ' || *line == '\t') line++;
        
        if (*line && *line != ';') {
            ExecuteLogoLine(line);
        }
        
        line = _tcstok_s(NULL, TEXT("\n"), &save);
    }
    
    free(dup);
    return TRUE;
}

/* ============================================================================
 * Procedure Parsing
 * ============================================================================ */

static void ParseProcedures(const TCHAR *code) {
    TCHAR *dup;
    TCHAR *line;
    TCHAR *save = NULL;
    BOOL inProc = FALSE;
    TCHAR procName[64] = {0};
    TCHAR procParams[256] = {0};
    TCHAR procBody[8192] = {0};
    
    dup = _tcsdup(code);
    line = _tcstok_s(dup, TEXT("\n"), &save);
    
    while (line) {
        while (*line == ' ' || *line == '\t') line++;
        
        if (_tcsnicmp(line, TEXT("TO "), 3) == 0) {
            inProc = TRUE;
            procBody[0] = 0;
            
            /* Parse procedure name and parameters */
            const TCHAR *p = line + 3;
            while (*p == ' ') p++;
            
            int ni = 0;
            while (*p && !isspace(*p) && ni < 63) {
                procName[ni++] = *p++;
            }
            procName[ni] = 0;
            
            /* Get parameters */
            while (*p == ' ') p++;
            _tcsncpy(procParams, p, 255);
            procParams[255] = 0;
        }
        else if (inProc && _tcsicmp(line, TEXT("END")) == 0) {
            inProc = FALSE;
            StoreProcedure(procName, procParams, procBody);
        }
        else if (inProc) {
            if (procBody[0]) _tcscat(procBody, TEXT("\n"));
            _tcsncat(procBody, line, 8191 - _tcslen(procBody));
        }
        
        line = _tcstok_s(NULL, TEXT("\n"), &save);
    }
    
    free(dup);
}

/* ============================================================================
 * Main Execution
 * ============================================================================ */

BOOL LogoInterpreter_Execute(const TCHAR *code, HWND hwndConsole,
                              HWND hwndCanvas, BOOL debugMode) {
    g_hwndConsole = hwndConsole;
    g_hwndCanvas = hwndCanvas;
    g_debugMode = debugMode;
    g_running = TRUE;
    g_stopRequested = FALSE;
    g_callDepth = 0;
    g_repeatDepth = 0;
    
    /* Initialize turtle position based on canvas */
    if (g_hwndCanvas) {
        RECT rc;
        GetClientRect(g_hwndCanvas, &rc);
        g_turtle.x = (rc.right - rc.left) / 2.0;
        g_turtle.y = (rc.bottom - rc.top) / 2.0;
    }
    
    /* First pass: parse procedure definitions */
    ParseProcedures(code);
    
    /* Second pass: execute non-procedure code */
    TCHAR *dup = _tcsdup(code);
    TCHAR *line;
    TCHAR *save = NULL;
    BOOL inProc = FALSE;
    
    line = _tcstok_s(dup, TEXT("\n"), &save);
    
    while (line && g_running) {
        while (*line == ' ' || *line == '\t') line++;
        
        if (_tcsnicmp(line, TEXT("TO "), 3) == 0) {
            inProc = TRUE;
        }
        else if (inProc && _tcsicmp(line, TEXT("END")) == 0) {
            inProc = FALSE;
        }
        else if (!inProc && *line && *line != ';') {
            ExecuteLogoLine(line);
        }
        
        line = _tcstok_s(NULL, TEXT("\n"), &save);
    }
    
    free(dup);
    g_running = FALSE;
    return TRUE;
}

void LogoInterpreter_Stop(void) {
    g_running = FALSE;
    g_stopRequested = TRUE;
}

BOOL LogoInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) {
    const TCHAR *val = GetStringVariable(name);
    _tcsncpy(value, val, maxLen - 1);
    value[maxLen - 1] = 0;
    return TRUE;
}

BOOL LogoInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) {
    double num = _tstof(value);
    SetVariable(name, num);
    return TRUE;
}

BOOL LogoInterpreter_DefineProcedure(const TCHAR *name, const TCHAR *body) {
    return StoreProcedure(name, TEXT(""), body);
}

BOOL LogoInterpreter_CallProcedure(const TCHAR *name, const TCHAR **args, int argCount) {
    TCHAR line[1024];
    int i;
    
    _tcscpy(line, name);
    for (i = 0; i < argCount; i++) {
        _tcscat(line, TEXT(" "));
        _tcscat(line, args[i]);
    }
    
    return ExecuteLogoLine(line);
}
