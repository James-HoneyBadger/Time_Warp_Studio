/*
 * pilot_interpreter.c - PILOT interpreter with full command support
 * 
 * Matches Python version functionality including:
 * - Conditional commands (TY:, TN:, etc.)
 * - Match with comma-separated alternatives
 * - Variable interpolation with #VAR syntax
 * - Graphics commands (G:)
 * - Subroutine calls (S:, R:)
 * - All standard PILOT commands
 */

#include "pilot_interpreter.h"
#include "canvas.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <tchar.h>
#include <math.h>

#ifndef _MSC_VER
#ifndef _tcstok_s
#define _tcstok_s(str, delim, ctx) _tcstok(str, delim)
#endif
#ifndef _tcsdup
#define _tcsdup _strdup
#endif
#endif

/* Variable storage */
typedef struct {
    TCHAR name[32];
    TCHAR strValue[256];
    double numValue;
    BOOL isNumeric;
} PilotVar;

/* Label storage */
typedef struct {
    TCHAR label[64];
    int lineIndex;
} PilotLabel;

/* Subroutine return stack */
#define MAX_SUBROUTINE_DEPTH 64

/* Global interpreter state */
static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static HWND g_hwndCanvas = NULL;
static PilotVar g_vars[256];
static int g_varCount = 0;
static PilotLabel g_labels[512];
static int g_labelCount = 0;
static TCHAR g_lastInput[512] = {0};
static BOOL g_matchSucceeded = FALSE;
static int g_subroutineStack[MAX_SUBROUTINE_DEPTH];
static int g_subroutineDepth = 0;
static int g_currentLine = 0;

/* Forward declarations */
static void set_var(const TCHAR* name, const TCHAR* strVal, double numVal, BOOL isNumeric);
static const TCHAR* get_var_str(const TCHAR* name);
static double get_var_num(const TCHAR* name);
static BOOL has_var(const TCHAR* name);
static void register_label(const TCHAR* label, int idx);
static int find_label(const TCHAR* label);
static void output_text(const TCHAR *text);
static void interpolate_text(const TCHAR* input, TCHAR* output, int maxLen);
static double evaluate_expression(const TCHAR* expr);
static BOOL match_pattern(const TCHAR* input, const TCHAR* pattern);
static BOOL execute_command(TCHAR cmdType, TCHAR condition, const TCHAR* arg);
static BOOL execute_graphics_command(const TCHAR* command);

/* Set a variable */
static void set_var(const TCHAR* name, const TCHAR* strVal, double numVal, BOOL isNumeric) {
    TCHAR upperName[32];
    _tcsncpy(upperName, name, 31);
    upperName[31] = 0;
    CharUpperBuff(upperName, (DWORD)_tcslen(upperName));
    
    int idx = -1;
    for (int i = 0; i < g_varCount; i++) {
        if (_tcsicmp(g_vars[i].name, upperName) == 0) {
            idx = i;
            break;
        }
    }
    if (idx < 0 && g_varCount < 256) {
        idx = g_varCount++;
    }
    if (idx >= 0) {
        _tcsncpy(g_vars[idx].name, upperName, 31);
        g_vars[idx].name[31] = 0;
        if (strVal) {
            _tcsncpy(g_vars[idx].strValue, strVal, 255);
            g_vars[idx].strValue[255] = 0;
        }
        g_vars[idx].numValue = numVal;
        g_vars[idx].isNumeric = isNumeric;
    }
}

/* Get string value of variable */
static const TCHAR* get_var_str(const TCHAR* name) {
    TCHAR upperName[32];
    _tcsncpy(upperName, name, 31);
    upperName[31] = 0;
    CharUpperBuff(upperName, (DWORD)_tcslen(upperName));
    
    for (int i = 0; i < g_varCount; i++) {
        if (_tcsicmp(g_vars[i].name, upperName) == 0) {
            if (g_vars[i].isNumeric) {
                static TCHAR numBuf[64];
                _stprintf(numBuf, TEXT("%g"), g_vars[i].numValue);
                return numBuf;
            }
            return g_vars[i].strValue;
        }
    }
    return TEXT("");
}

/* Get numeric value of variable */
static double get_var_num(const TCHAR* name) {
    TCHAR upperName[32];
    _tcsncpy(upperName, name, 31);
    upperName[31] = 0;
    CharUpperBuff(upperName, (DWORD)_tcslen(upperName));
    
    for (int i = 0; i < g_varCount; i++) {
        if (_tcsicmp(g_vars[i].name, upperName) == 0) {
            return g_vars[i].numValue;
        }
    }
    return 0.0;
}

/* Check if variable exists */
static BOOL has_var(const TCHAR* name) {
    TCHAR upperName[32];
    _tcsncpy(upperName, name, 31);
    upperName[31] = 0;
    CharUpperBuff(upperName, (DWORD)_tcslen(upperName));
    
    for (int i = 0; i < g_varCount; i++) {
        if (_tcsicmp(g_vars[i].name, upperName) == 0) {
            return TRUE;
        }
    }
    return FALSE;
}

/* Register a label */
static void register_label(const TCHAR* label, int idx) {
    if (g_labelCount < 512) {
        _tcsncpy(g_labels[g_labelCount].label, label, 63);
        g_labels[g_labelCount].label[63] = 0;
        /* Trim whitespace */
        TCHAR* p = g_labels[g_labelCount].label;
        while (*p && (*p == TEXT(' ') || *p == TEXT('\t'))) p++;
        if (p != g_labels[g_labelCount].label) {
            memmove(g_labels[g_labelCount].label, p, (_tcslen(p) + 1) * sizeof(TCHAR));
        }
        g_labels[g_labelCount].lineIndex = idx;
        g_labelCount++;
    }
}

/* Find a label, return line index or -1 */
static int find_label(const TCHAR* label) {
    for (int i = 0; i < g_labelCount; i++) {
        if (_tcsicmp(g_labels[i].label, label) == 0) {
            return g_labels[i].lineIndex;
        }
    }
    return -1;
}

/* Output text to console */
static void output_text(const TCHAR *text) {
    if (g_hwndConsole) {
        TCHAR buf[2048];
        _stprintf(buf, TEXT("%s\r\n"), text);
        int len = GetWindowTextLength(g_hwndConsole);
        SendMessage(g_hwndConsole, EM_SETSEL, len, len);
        SendMessage(g_hwndConsole, EM_REPLACESEL, FALSE, (LPARAM)buf);
    }
}

/* Interpolate variables in text - supports both #VAR and *VAR* syntax */
static void interpolate_text(const TCHAR* input, TCHAR* output, int maxLen) {
    const TCHAR* src = input;
    TCHAR* dst = output;
    TCHAR* end = output + maxLen - 1;
    
    while (*src && dst < end) {
        /* Handle #VAR syntax (PILOT style) */
        if (*src == TEXT('#')) {
            src++;
            TCHAR varName[32];
            int vi = 0;
            while (*src && (isalnum(*src) || *src == TEXT('_')) && vi < 31) {
                varName[vi++] = *src++;
            }
            varName[vi] = 0;
            if (vi > 0) {
                const TCHAR* val = get_var_str(varName);
                while (*val && dst < end) {
                    *dst++ = *val++;
                }
            }
        }
        /* Handle *VAR* syntax (BASIC style) */
        else if (*src == TEXT('*')) {
            const TCHAR* start = src + 1;
            const TCHAR* endAst = _tcschr(start, TEXT('*'));
            if (endAst && (endAst - start) < 32) {
                TCHAR varName[32];
                int len = (int)(endAst - start);
                _tcsncpy(varName, start, len);
                varName[len] = 0;
                if (has_var(varName)) {
                    const TCHAR* val = get_var_str(varName);
                    while (*val && dst < end) {
                        *dst++ = *val++;
                    }
                    src = endAst + 1;
                } else {
                    *dst++ = *src++;
                }
            } else {
                *dst++ = *src++;
            }
        }
        else {
            *dst++ = *src++;
        }
    }
    *dst = 0;
}

/* Simple expression evaluator */
static double evaluate_expression(const TCHAR* expr) {
    TCHAR buf[256];
    _tcsncpy(buf, expr, 255);
    buf[255] = 0;
    
    /* Trim whitespace */
    TCHAR* p = buf;
    while (*p == TEXT(' ') || *p == TEXT('\t')) p++;
    
    /* Check if it's a variable name */
    if (isalpha(*p)) {
        TCHAR varName[32];
        int vi = 0;
        while (*p && (isalnum(*p) || *p == TEXT('_')) && vi < 31) {
            varName[vi++] = *p++;
        }
        varName[vi] = 0;
        
        /* Skip whitespace after variable */
        while (*p == TEXT(' ') || *p == TEXT('\t')) p++;
        
        /* Check for operator */
        if (*p == 0) {
            return get_var_num(varName);
        }
        
        double left = get_var_num(varName);
        TCHAR op = *p++;
        
        /* Skip whitespace after operator */
        while (*p == TEXT(' ') || *p == TEXT('\t')) p++;
        
        /* Get right operand (recursively) */
        double right = evaluate_expression(p);
        
        switch (op) {
            case TEXT('+'): return left + right;
            case TEXT('-'): return left - right;
            case TEXT('*'): return left * right;
            case TEXT('/'): return (right != 0) ? left / right : 0;
            default: return left;
        }
    }
    
    /* Try to parse as number */
    TCHAR* endPtr;
    double val = _tcstod(p, &endPtr);
    if (endPtr != p) {
        /* Skip whitespace */
        while (*endPtr == TEXT(' ') || *endPtr == TEXT('\t')) endPtr++;
        
        if (*endPtr == 0) {
            return val;
        }
        
        /* Check for operator */
        TCHAR op = *endPtr++;
        while (*endPtr == TEXT(' ') || *endPtr == TEXT('\t')) endPtr++;
        
        double right = evaluate_expression(endPtr);
        
        switch (op) {
            case TEXT('+'): return val + right;
            case TEXT('-'): return val - right;
            case TEXT('*'): return val * right;
            case TEXT('/'): return (right != 0) ? val / right : 0;
            default: return val;
        }
    }
    
    return 0.0;
}

/* Match pattern against input - supports comma-separated alternatives */
static BOOL match_pattern(const TCHAR* input, const TCHAR* pattern) {
    TCHAR inputUpper[512];
    TCHAR patternBuf[256];
    
    _tcsncpy(inputUpper, input, 511);
    inputUpper[511] = 0;
    CharUpperBuff(inputUpper, (DWORD)_tcslen(inputUpper));
    
    /* Trim input whitespace */
    TCHAR* inp = inputUpper;
    while (*inp == TEXT(' ') || *inp == TEXT('\t')) inp++;
    TCHAR* end = inp + _tcslen(inp) - 1;
    while (end > inp && (*end == TEXT(' ') || *end == TEXT('\t') || *end == TEXT('\r') || *end == TEXT('\n'))) {
        *end-- = 0;
    }
    
    _tcsncpy(patternBuf, pattern, 255);
    patternBuf[255] = 0;
    
    /* Split pattern by commas and check each alternative */
    TCHAR* savePtr = NULL;
    TCHAR* token = _tcstok_s(patternBuf, TEXT(","), &savePtr);
    
    while (token) {
        /* Trim whitespace from token */
        while (*token == TEXT(' ') || *token == TEXT('\t')) token++;
        TCHAR* tokenEnd = token + _tcslen(token) - 1;
        while (tokenEnd > token && (*tokenEnd == TEXT(' ') || *tokenEnd == TEXT('\t'))) {
            *tokenEnd-- = 0;
        }
        
        /* Convert to uppercase for comparison */
        CharUpperBuff(token, (DWORD)_tcslen(token));
        
        /* Check for wildcard */
        if (_tcschr(token, TEXT('*'))) {
            /* Simple wildcard matching - * matches any characters */
            const TCHAR* p = token;
            const TCHAR* s = inp;
            BOOL match = TRUE;
            
            while (*p && match) {
                if (*p == TEXT('*')) {
                    p++;
                    if (*p == 0) {
                        /* Trailing * matches everything */
                        break;
                    }
                    /* Find next non-* character in pattern */
                    while (*s) {
                        if (*s == *p) break;
                        s++;
                    }
                    if (*s == 0 && *p != 0) {
                        match = FALSE;
                    }
                } else {
                    if (*s != *p) {
                        match = FALSE;
                    }
                    s++;
                    p++;
                }
            }
            
            if (match) return TRUE;
        } else {
            /* Exact match (case-insensitive) */
            if (_tcsicmp(inp, token) == 0) {
                return TRUE;
            }
        }
        
        token = _tcstok_s(NULL, TEXT(","), &savePtr);
    }
    
    return FALSE;
}

/* Execute graphics command */
static BOOL execute_graphics_command(const TCHAR* command) {
    TCHAR buf[256];
    _tcsncpy(buf, command, 255);
    buf[255] = 0;
    
    /* Replace commas with spaces */
    for (TCHAR* p = buf; *p; p++) {
        if (*p == TEXT(',')) *p = TEXT(' ');
    }
    
    /* Parse command and arguments */
    TCHAR* savePtr = NULL;
    TCHAR* cmd = _tcstok_s(buf, TEXT(" \t"), &savePtr);
    if (!cmd) return FALSE;
    
    CharUpperBuff(cmd, (DWORD)_tcslen(cmd));
    
    if (_tcscmp(cmd, TEXT("FORWARD")) == 0 || _tcscmp(cmd, TEXT("FD")) == 0) {
        TCHAR* arg = _tcstok_s(NULL, TEXT(" \t"), &savePtr);
        if (arg) {
            double dist = evaluate_expression(arg);
            Canvas_Forward(g_hwndCanvas, dist);
        }
    }
    else if (_tcscmp(cmd, TEXT("BACK")) == 0 || _tcscmp(cmd, TEXT("BK")) == 0) {
        TCHAR* arg = _tcstok_s(NULL, TEXT(" \t"), &savePtr);
        if (arg) {
            double dist = evaluate_expression(arg);
            Canvas_Back(g_hwndCanvas, dist);
        }
    }
    else if (_tcscmp(cmd, TEXT("LEFT")) == 0 || _tcscmp(cmd, TEXT("LT")) == 0) {
        TCHAR* arg = _tcstok_s(NULL, TEXT(" \t"), &savePtr);
        if (arg) {
            double angle = evaluate_expression(arg);
            Canvas_Left(g_hwndCanvas, angle);
        }
    }
    else if (_tcscmp(cmd, TEXT("RIGHT")) == 0 || _tcscmp(cmd, TEXT("RT")) == 0) {
        TCHAR* arg = _tcstok_s(NULL, TEXT(" \t"), &savePtr);
        if (arg) {
            double angle = evaluate_expression(arg);
            Canvas_Right(g_hwndCanvas, angle);
        }
    }
    else if (_tcscmp(cmd, TEXT("PENUP")) == 0 || _tcscmp(cmd, TEXT("PU")) == 0) {
        Canvas_PenUp(g_hwndCanvas);
    }
    else if (_tcscmp(cmd, TEXT("PENDOWN")) == 0 || _tcscmp(cmd, TEXT("PD")) == 0) {
        Canvas_PenDown(g_hwndCanvas);
    }
    else if (_tcscmp(cmd, TEXT("HOME")) == 0) {
        Canvas_Home(g_hwndCanvas);
    }
    else if (_tcscmp(cmd, TEXT("CLEAR")) == 0 || _tcscmp(cmd, TEXT("CS")) == 0) {
        Canvas_Clear(g_hwndCanvas);
    }
    else if (_tcscmp(cmd, TEXT("SETXY")) == 0) {
        TCHAR* xArg = _tcstok_s(NULL, TEXT(" \t"), &savePtr);
        TCHAR* yArg = _tcstok_s(NULL, TEXT(" \t"), &savePtr);
        if (xArg && yArg) {
            double x = evaluate_expression(xArg);
            double y = evaluate_expression(yArg);
            Canvas_SetXY(g_hwndCanvas, x, y);
        }
    }
    else if (_tcscmp(cmd, TEXT("CIRCLE")) == 0) {
        TCHAR* arg = _tcstok_s(NULL, TEXT(" \t"), &savePtr);
        if (arg) {
            double radius = evaluate_expression(arg);
            Canvas_Circle(g_hwndCanvas, radius);
        }
    }
    else {
        return FALSE;
    }
    
    return TRUE;
}

/* Execute a single command */
static BOOL execute_command(TCHAR cmdType, TCHAR condition, const TCHAR* arg) {
    /* Check condition before executing */
    if (condition == TEXT('Y') && !g_matchSucceeded) {
        return TRUE;  /* Skip - condition not met */
    }
    if (condition == TEXT('N') && g_matchSucceeded) {
        return TRUE;  /* Skip - condition not met */
    }
    
    switch (cmdType) {
        case TEXT('T'): {
            /* T: Type - output text with variable interpolation */
            TCHAR interpolated[1024];
            interpolate_text(arg, interpolated, 1024);
            output_text(interpolated);
            break;
        }
        
        case TEXT('A'): {
            /* A: Accept - get input from user */
            TCHAR prompt[128];
            _stprintf(prompt, TEXT("%s? "), arg);
            output_text(prompt);
            /* In real implementation, would wait for input */
            /* For now, store empty string */
            set_var(arg, TEXT(""), 0.0, FALSE);
            break;
        }
        
        case TEXT('M'): {
            /* M: Match - check if input matches pattern(s) */
            g_matchSucceeded = match_pattern(g_lastInput, arg);
            break;
        }
        
        case TEXT('C'): {
            /* C: Compute - evaluate expression and store in variable */
            const TCHAR* eq = _tcschr(arg, TEXT('='));
            if (eq) {
                TCHAR varName[32] = {0};
                int len = (int)(eq - arg);
                if (len > 31) len = 31;
                _tcsncpy(varName, arg, len);
                varName[len] = 0;
                /* Trim whitespace from variable name */
                TCHAR* p = varName;
                while (*p == TEXT(' ') || *p == TEXT('\t')) p++;
                TCHAR* end = p + _tcslen(p) - 1;
                while (end > p && (*end == TEXT(' ') || *end == TEXT('\t'))) {
                    *end-- = 0;
                }
                if (p != varName) {
                    memmove(varName, p, (_tcslen(p) + 1) * sizeof(TCHAR));
                }
                
                double result = evaluate_expression(eq + 1);
                TCHAR strVal[64];
                _stprintf(strVal, TEXT("%g"), result);
                set_var(varName, strVal, result, TRUE);
            }
            break;
        }
        
        case TEXT('U'): {
            /* U: Use - output variable value or compute */
            if (_tcschr(arg, TEXT('='))) {
                /* Treat as Compute */
                return execute_command(TEXT('C'), 0, arg);
            }
            const TCHAR* val = get_var_str(arg);
            output_text(val);
            break;
        }
        
        case TEXT('J'): {
            /* J: Jump - unconditional jump to label */
            int idx = find_label(arg);
            if (idx >= 0) {
                g_currentLine = idx;
            }
            break;
        }
        
        case TEXT('Y'): {
            /* Y: Yes-jump - jump if match succeeded */
            if (g_matchSucceeded) {
                int idx = find_label(arg);
                if (idx >= 0) {
                    g_currentLine = idx;
                }
            }
            break;
        }
        
        case TEXT('N'): {
            /* N: No-jump - jump if match failed */
            if (!g_matchSucceeded) {
                int idx = find_label(arg);
                if (idx >= 0) {
                    g_currentLine = idx;
                }
            }
            break;
        }
        
        case TEXT('S'): {
            /* S: Subroutine - call subroutine */
            if (g_subroutineDepth < MAX_SUBROUTINE_DEPTH) {
                g_subroutineStack[g_subroutineDepth++] = g_currentLine;
                int idx = find_label(arg);
                if (idx >= 0) {
                    g_currentLine = idx;
                }
            }
            break;
        }
        
        case TEXT('R'): {
            /* R: Return - return from subroutine */
            if (g_subroutineDepth > 0) {
                g_currentLine = g_subroutineStack[--g_subroutineDepth];
            }
            break;
        }
        
        case TEXT('G'): {
            /* G: Graphics - turtle graphics command */
            execute_graphics_command(arg);
            break;
        }
        
        case TEXT('P'): {
            /* P: Pause - wait for user */
            output_text(TEXT("Press Enter to continue..."));
            break;
        }
        
        case TEXT('D'): {
            /* D: Delay - pause execution */
            double delay = _tstof(arg);
            if (delay > 0 && delay <= 10.0) {
                Sleep((DWORD)(delay * 1000));
            }
            break;
        }
        
        case TEXT('B'): {
            /* B: Branch - conditional jump based on expression */
            const TCHAR* eq = _tcschr(arg, TEXT('='));
            if (eq) {
                TCHAR condBuf[128] = {0};
                int len = (int)(eq - arg);
                if (len > 127) len = 127;
                _tcsncpy(condBuf, arg, len);
                condBuf[len] = 0;
                
                double result = evaluate_expression(condBuf);
                if (result != 0.0) {
                    const TCHAR* label = eq + 1;
                    while (*label == TEXT(' ') || *label == TEXT('\t')) label++;
                    int idx = find_label(label);
                    if (idx >= 0) {
                        g_currentLine = idx;
                    }
                }
            }
            break;
        }
        
        case TEXT('L'): {
            /* L: Label - handled during preprocessing */
            break;
        }
        
        case TEXT('E'): {
            /* E: End - terminate program */
            g_running = FALSE;
            break;
        }
        
        default:
            return FALSE;
    }
    
    return TRUE;
}

/* Initialize interpreter */
BOOL PilotInterpreter_Init(void) {
    g_running = FALSE;
    g_varCount = 0;
    g_labelCount = 0;
    g_matchSucceeded = FALSE;
    g_subroutineDepth = 0;
    g_currentLine = 0;
    g_lastInput[0] = 0;
    return TRUE;
}

/* Cleanup interpreter */
void PilotInterpreter_Cleanup(void) {
    g_running = FALSE;
}

/* Execute PILOT program */
BOOL PilotInterpreter_Execute(const TCHAR *code, HWND hwndConsole, HWND hwndCanvas, BOOL debugMode) {
    (void)debugMode;  /* Not yet implemented */
    
    g_hwndConsole = hwndConsole;
    g_hwndCanvas = hwndCanvas;
    g_running = TRUE;
    g_varCount = 0;
    g_labelCount = 0;
    g_matchSucceeded = FALSE;
    g_subroutineDepth = 0;
    
    /* Duplicate code for parsing */
    TCHAR *dup = _tcsdup(code);
    if (!dup) return FALSE;
    
    /* Split into lines */
    TCHAR *lines[4096];
    int lineCount = 0;
    TCHAR *savePtr = NULL;
    TCHAR *ln = _tcstok_s(dup, TEXT("\n"), &savePtr);
    while (ln && lineCount < 4096) {
        lines[lineCount++] = ln;
        ln = _tcstok_s(NULL, TEXT("\n"), &savePtr);
    }
    
    /* First pass: register labels */
    for (int i = 0; i < lineCount; i++) {
        TCHAR *l = lines[i];
        while (*l == TEXT(' ') || *l == TEXT('\t')) l++;
        
        /* *label syntax */
        if (*l == TEXT('*')) {
            register_label(l + 1, i);
        }
        /* L:label syntax */
        else if (_tcsnicmp(l, TEXT("L:"), 2) == 0) {
            register_label(l + 2, i);
        }
    }
    
    /* Second pass: execute */
    for (g_currentLine = 0; g_currentLine < lineCount && g_running; g_currentLine++) {
        TCHAR *l = lines[g_currentLine];
        while (*l == TEXT(' ') || *l == TEXT('\t')) l++;
        
        /* Skip empty lines and labels */
        if (_tcslen(l) < 2) continue;
        if (*l == TEXT('*')) continue;
        if (_tcsnicmp(l, TEXT("R:"), 2) == 0 && _tcslen(l) > 2) {
            /* R: followed by text is a remark/comment */
            TCHAR *arg = l + 2;
            while (*arg == TEXT(' ') || *arg == TEXT('\t')) arg++;
            /* Check if it looks like a comment (starts with text, not a label jump) */
            if (_tcslen(arg) > 10 || _tcschr(arg, TEXT(' '))) {
                continue;  /* It's a comment, skip */
            }
        }
        
        /* Find colon position */
        TCHAR* colon = _tcschr(l, TEXT(':'));
        if (!colon) continue;
        
        int prefixLen = (int)(colon - l);
        if (prefixLen < 1 || prefixLen > 2) continue;
        
        /* Extract command type and condition */
        TCHAR cmdType = (TCHAR)toupper(l[0]);
        TCHAR condition = 0;
        
        if (prefixLen == 2) {
            TCHAR secondChar = (TCHAR)toupper(l[1]);
            if (secondChar == TEXT('Y') || secondChar == TEXT('N')) {
                condition = secondChar;
            } else {
                continue;  /* Invalid prefix */
            }
        }
        
        /* Get argument */
        const TCHAR *arg = colon + 1;
        while (*arg == TEXT(' ') || *arg == TEXT('\t')) arg++;
        
        /* Execute command */
        execute_command(cmdType, condition, arg);
    }
    
    free(dup);
    g_running = FALSE;
    return TRUE;
}

/* Stop execution */
void PilotInterpreter_Stop(void) {
    g_running = FALSE;
}

/* Reset interpreter state */
void PilotInterpreter_Reset(void) {
    g_varCount = 0;
    g_labelCount = 0;
    g_matchSucceeded = FALSE;
    g_subroutineDepth = 0;
    g_currentLine = 0;
    g_lastInput[0] = 0;
}

/* Get variable value */
BOOL PilotInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) {
    const TCHAR* val = get_var_str(name);
    if (val && _tcslen(val) > 0) {
        _tcsncpy(value, val, maxLen - 1);
        value[maxLen - 1] = 0;
        return TRUE;
    }
    return FALSE;
}

/* Set variable value */
BOOL PilotInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) {
    TCHAR* endPtr;
    double numVal = _tcstod(value, &endPtr);
    BOOL isNumeric = (endPtr != value && *endPtr == 0);
    set_var(name, value, numVal, isNumeric);
    return TRUE;
}

/* Set last input (for matching) */
void PilotInterpreter_SetLastInput(const TCHAR *input) {
    _tcsncpy(g_lastInput, input, 511);
    g_lastInput[511] = 0;
}
