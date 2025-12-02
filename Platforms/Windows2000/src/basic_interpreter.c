/*
 * basic_interpreter.c - Complete BASIC Interpreter for Time Warp IDE
 * Matches Python implementation features:
 * - Typed variables (INTEGER%, LONG&, SINGLE!, DOUBLE#, STRING$)
 * - Arrays with DIM
 * - FOR/NEXT, WHILE/WEND, DO/LOOP
 * - GOSUB/RETURN
 * - IF/THEN/ELSE
 * - SELECT CASE
 * - SUB/FUNCTION
 * - DATA/READ/RESTORE
 * - Graphics: LINE, CIRCLE, CLS, COLOR, LOCATE, SCREEN
 * - File I/O: OPEN, CLOSE, GET, PUT
 * - String functions: LEN, MID$, LEFT$, RIGHT$, CHR$, ASC, VAL, STR$
 * - Math functions: SIN, COS, TAN, ATN, SQR, ABS, INT, RND, SGN, LOG, EXP
 * - Special: INKEY$, TIME$, DATE$, TIMER, BEEP, SOUND
 * - Variable interpolation: *VAR* syntax
 */

#include "basic_interpreter.h"
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

/* Numeric variable types */
typedef enum {
    VAR_TYPE_INTEGER,   /* % suffix */
    VAR_TYPE_LONG,      /* & suffix */
    VAR_TYPE_SINGLE,    /* ! suffix */
    VAR_TYPE_DOUBLE,    /* # suffix (default) */
    VAR_TYPE_STRING     /* $ suffix */
} VarType;

typedef struct {
    TCHAR name[64];
    VarType type;
    union {
        int intVal;
        long longVal;
        float singleVal;
        double doubleVal;
        TCHAR stringVal[1024];
    } value;
} Variable;

typedef struct {
    TCHAR name[64];
    int dimensions[4];
    int numDimensions;
    double *data;
    int size;
} ArrayVar;

typedef struct {
    TCHAR varName[32];
    double endValue;
    double step;
    int forLine;
} ForContext;

typedef struct {
    TCHAR condition[256];
} WhileContext;

typedef struct {
    TCHAR type[16]; /* "WHILE" or "UNTIL" */
    TCHAR condition[256];
    int startLine;
} DoContext;

typedef struct {
    TCHAR name[64];
    TCHAR params[256];
    int startLine;
    int endLine;
} SubroutineInfo;

/* ============================================================================
 * Global State
 * ============================================================================ */

#define MAX_VARIABLES 512
#define MAX_ARRAYS 64
#define MAX_FOR_DEPTH 32
#define MAX_WHILE_DEPTH 32
#define MAX_DO_DEPTH 32
#define MAX_GOSUB_DEPTH 64
#define MAX_LINES 4096
#define MAX_DATA_VALUES 1024
#define MAX_SUBS 64

static Variable g_variables[MAX_VARIABLES];
static int g_varCount = 0;

static ArrayVar g_arrays[MAX_ARRAYS];
static int g_arrayCount = 0;

static ForContext g_forStack[MAX_FOR_DEPTH];
static int g_forStackPtr = 0;

static WhileContext g_whileStack[MAX_WHILE_DEPTH];
static int g_whileStackPtr = 0;

static DoContext g_doStack[MAX_DO_DEPTH];
static int g_doStackPtr = 0;

static int g_gosubStack[MAX_GOSUB_DEPTH];
static int g_gosubStackPtr = 0;

static SubroutineInfo g_subs[MAX_SUBS];
static int g_subCount = 0;

static TCHAR *g_programLines[MAX_LINES];
static int g_lineNumbers[MAX_LINES];
static int g_totalLines = 0;
static int g_currentLine = 0;

static TCHAR g_dataValues[MAX_DATA_VALUES][256];
static int g_dataCount = 0;
static int g_dataPointer = 0;

static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static HWND g_hwndCanvas = NULL;
static BOOL g_debugMode = FALSE;

static int g_cursorRow = 0;
static int g_cursorCol = 0;
static COLORREF g_fgColor = RGB(255, 255, 255);
static COLORREF g_bgColor = RGB(0, 0, 0);

/* Default type map for variables A-Z */
static VarType g_defaultTypes[26];

/* Random number state */
static unsigned int g_rndSeed = 1;

/* ============================================================================
 * Forward Declarations
 * ============================================================================ */

static void Console_Write(const TCHAR *text);
static void Console_WriteLine(const TCHAR *text);
static double EvaluateExpression(const TCHAR *expr);
static BOOL EvaluateCondition(const TCHAR *cond);
static void SetVariable(const TCHAR *name, const TCHAR *value);
static const TCHAR *GetVariable(const TCHAR *name);
static double GetNumericVariable(const TCHAR *name);
static void SetNumericVariable(const TCHAR *name, double value);
static TCHAR *InterpolateText(const TCHAR *text, TCHAR *buffer, int bufSize);
static int FindLineIndex(int lineNum);
static int FindLabel(const TCHAR *label);
static void ExecuteCommand(const TCHAR *line);

/* ============================================================================
 * Initialization
 * ============================================================================ */

BOOL BasicInterpreter_Init(void) {
    int i;
    g_varCount = 0;
    g_arrayCount = 0;
    g_forStackPtr = 0;
    g_whileStackPtr = 0;
    g_doStackPtr = 0;
    g_gosubStackPtr = 0;
    g_subCount = 0;
    g_totalLines = 0;
    g_currentLine = 0;
    g_dataCount = 0;
    g_dataPointer = 0;
    g_running = FALSE;
    g_cursorRow = 0;
    g_cursorCol = 0;
    g_rndSeed = (unsigned int)time(NULL);
    
    /* Default all variables to DOUBLE */
    for (i = 0; i < 26; i++) {
        g_defaultTypes[i] = VAR_TYPE_DOUBLE;
    }
    
    return TRUE;
}

void BasicInterpreter_Cleanup(void) {
    int i;
    
    /* Free arrays */
    for (i = 0; i < g_arrayCount; i++) {
        if (g_arrays[i].data) {
            free(g_arrays[i].data);
            g_arrays[i].data = NULL;
        }
    }
    g_arrayCount = 0;
    
    /* Free program lines */
    for (i = 0; i < g_totalLines; i++) {
        if (g_programLines[i]) {
            free(g_programLines[i]);
            g_programLines[i] = NULL;
        }
    }
    g_totalLines = 0;
    
    g_varCount = 0;
}

void BasicInterpreter_Reset(void) {
    BasicInterpreter_Cleanup();
    BasicInterpreter_Init();
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
 * Variable Type Resolution
 * ============================================================================ */

static VarType GetVarType(const TCHAR *name) {
    int len;
    TCHAR lastChar;
    
    if (!name || !*name) return VAR_TYPE_DOUBLE;
    
    len = _tcslen(name);
    lastChar = name[len - 1];
    
    switch (lastChar) {
        case '%': return VAR_TYPE_INTEGER;
        case '&': return VAR_TYPE_LONG;
        case '!': return VAR_TYPE_SINGLE;
        case '#': return VAR_TYPE_DOUBLE;
        case '$': return VAR_TYPE_STRING;
        default:
            /* Use default type based on first letter */
            if (name[0] >= 'A' && name[0] <= 'Z') {
                return g_defaultTypes[name[0] - 'A'];
            }
            if (name[0] >= 'a' && name[0] <= 'z') {
                return g_defaultTypes[name[0] - 'a'];
            }
            return VAR_TYPE_DOUBLE;
    }
}

static void GetVarBaseName(const TCHAR *name, TCHAR *base, int maxLen) {
    int len;
    
    if (!name || !*name) {
        base[0] = 0;
        return;
    }
    
    len = _tcslen(name);
    _tcsncpy(base, name, maxLen - 1);
    base[maxLen - 1] = 0;
    
    /* Remove type suffix */
    len = _tcslen(base);
    if (len > 0) {
        TCHAR last = base[len - 1];
        if (last == '%' || last == '&' || last == '!' || last == '#' || last == '$') {
            base[len - 1] = 0;
        }
    }
}

/* ============================================================================
 * Variable Access
 * ============================================================================ */

static int FindVariable(const TCHAR *name) {
    int i;
    TCHAR baseName[64], searchBase[64];
    
    GetVarBaseName(name, baseName, 64);
    
    for (i = 0; i < g_varCount; i++) {
        GetVarBaseName(g_variables[i].name, searchBase, 64);
        if (_tcsicmp(searchBase, baseName) == 0) {
            return i;
        }
    }
    return -1;
}

static void SetVariable(const TCHAR *name, const TCHAR *value) {
    int idx;
    VarType type;
    TCHAR upperName[64];
    
    _tcsncpy(upperName, name, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    type = GetVarType(upperName);
    idx = FindVariable(upperName);
    
    if (idx < 0 && g_varCount < MAX_VARIABLES) {
        idx = g_varCount++;
        _tcscpy(g_variables[idx].name, upperName);
    }
    
    if (idx >= 0) {
        g_variables[idx].type = type;
        
        switch (type) {
            case VAR_TYPE_STRING:
                _tcsncpy(g_variables[idx].value.stringVal, value, 1023);
                g_variables[idx].value.stringVal[1023] = 0;
                break;
            case VAR_TYPE_INTEGER:
                g_variables[idx].value.intVal = (int)_tstof(value);
                break;
            case VAR_TYPE_LONG:
                g_variables[idx].value.longVal = (long)_tstof(value);
                break;
            case VAR_TYPE_SINGLE:
                g_variables[idx].value.singleVal = (float)_tstof(value);
                break;
            case VAR_TYPE_DOUBLE:
            default:
                g_variables[idx].value.doubleVal = _tstof(value);
                break;
        }
    }
}

static void SetNumericVariable(const TCHAR *name, double value) {
    TCHAR buf[64];
    _stprintf(buf, TEXT("%g"), value);
    SetVariable(name, buf);
}

static const TCHAR *GetVariable(const TCHAR *name) {
    static TCHAR result[1024];
    int idx;
    TCHAR upperName[64];
    
    _tcsncpy(upperName, name, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    idx = FindVariable(upperName);
    
    if (idx < 0) {
        /* Return "0" for undefined numeric, "" for undefined string */
        if (GetVarType(upperName) == VAR_TYPE_STRING) {
            return TEXT("");
        }
        return TEXT("0");
    }
    
    switch (g_variables[idx].type) {
        case VAR_TYPE_STRING:
            return g_variables[idx].value.stringVal;
        case VAR_TYPE_INTEGER:
            _stprintf(result, TEXT("%d"), g_variables[idx].value.intVal);
            break;
        case VAR_TYPE_LONG:
            _stprintf(result, TEXT("%ld"), g_variables[idx].value.longVal);
            break;
        case VAR_TYPE_SINGLE:
            _stprintf(result, TEXT("%g"), g_variables[idx].value.singleVal);
            break;
        case VAR_TYPE_DOUBLE:
        default:
            _stprintf(result, TEXT("%g"), g_variables[idx].value.doubleVal);
            break;
    }
    
    return result;
}

static double GetNumericVariable(const TCHAR *name) {
    int idx;
    TCHAR upperName[64];
    
    _tcsncpy(upperName, name, 63);
    upperName[63] = 0;
    _tcsupr(upperName);
    
    idx = FindVariable(upperName);
    
    if (idx < 0) return 0.0;
    
    switch (g_variables[idx].type) {
        case VAR_TYPE_STRING:
            return _tstof(g_variables[idx].value.stringVal);
        case VAR_TYPE_INTEGER:
            return (double)g_variables[idx].value.intVal;
        case VAR_TYPE_LONG:
            return (double)g_variables[idx].value.longVal;
        case VAR_TYPE_SINGLE:
            return (double)g_variables[idx].value.singleVal;
        case VAR_TYPE_DOUBLE:
        default:
            return g_variables[idx].value.doubleVal;
    }
}

/* ============================================================================
 * Array Support
 * ============================================================================ */

static int FindArray(const TCHAR *name) {
    int i;
    for (i = 0; i < g_arrayCount; i++) {
        if (_tcsicmp(g_arrays[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

static BOOL CreateArray(const TCHAR *name, int *dims, int numDims) {
    int i, size;
    ArrayVar *arr;
    
    if (g_arrayCount >= MAX_ARRAYS) return FALSE;
    
    arr = &g_arrays[g_arrayCount];
    _tcsncpy(arr->name, name, 63);
    arr->name[63] = 0;
    _tcsupr(arr->name);
    
    arr->numDimensions = numDims;
    size = 1;
    for (i = 0; i < numDims; i++) {
        arr->dimensions[i] = dims[i] + 1; /* BASIC arrays are 0 to n inclusive */
        size *= arr->dimensions[i];
    }
    
    arr->size = size;
    arr->data = (double *)calloc(size, sizeof(double));
    if (!arr->data) return FALSE;
    
    g_arrayCount++;
    return TRUE;
}

static double GetArrayElement(const TCHAR *name, int *indices, int numIndices) {
    int idx, arrIdx, offset, i, mult;
    
    arrIdx = FindArray(name);
    if (arrIdx < 0) return 0.0;
    
    offset = 0;
    mult = 1;
    for (i = numIndices - 1; i >= 0; i--) {
        if (indices[i] < 0 || indices[i] >= g_arrays[arrIdx].dimensions[i]) {
            return 0.0; /* Subscript out of range */
        }
        offset += indices[i] * mult;
        mult *= g_arrays[arrIdx].dimensions[i];
    }
    
    if (offset >= 0 && offset < g_arrays[arrIdx].size) {
        return g_arrays[arrIdx].data[offset];
    }
    return 0.0;
}

static void SetArrayElement(const TCHAR *name, int *indices, int numIndices, double value) {
    int idx, arrIdx, offset, i, mult;
    
    arrIdx = FindArray(name);
    if (arrIdx < 0) return;
    
    offset = 0;
    mult = 1;
    for (i = numIndices - 1; i >= 0; i--) {
        if (indices[i] < 0 || indices[i] >= g_arrays[arrIdx].dimensions[i]) {
            return; /* Subscript out of range */
        }
        offset += indices[i] * mult;
        mult *= g_arrays[arrIdx].dimensions[i];
    }
    
    if (offset >= 0 && offset < g_arrays[arrIdx].size) {
        g_arrays[arrIdx].data[offset] = value;
    }
}

/* ============================================================================
 * Expression Evaluation (Shunting-Yard Algorithm)
 * ============================================================================ */

typedef struct {
    const TCHAR *s;
} Lexer;

static void LexerSkipWhitespace(Lexer *lx) {
    while (*lx->s == ' ' || *lx->s == '\t') lx->s++;
}

static BOOL LexerReadNumber(Lexer *lx, double *out) {
    const TCHAR *start;
    BOOL hasDot = FALSE;
    TCHAR buf[64];
    int n;
    
    LexerSkipWhitespace(lx);
    start = lx->s;
    
    if (*lx->s == '-' || *lx->s == '+') lx->s++;
    
    while ((*lx->s >= '0' && *lx->s <= '9') || *lx->s == '.') {
        if (*lx->s == '.') {
            if (hasDot) break;
            hasDot = TRUE;
        }
        lx->s++;
    }
    
    if (lx->s > start) {
        n = (int)(lx->s - start);
        if (n > 63) n = 63;
        _tcsncpy(buf, start, n);
        buf[n] = 0;
        *out = _tstof(buf);
        return TRUE;
    }
    return FALSE;
}

static BOOL LexerReadIdent(Lexer *lx, TCHAR *out, int maxLen) {
    const TCHAR *start;
    int n;
    
    LexerSkipWhitespace(lx);
    start = lx->s;
    
    if ((*lx->s >= 'A' && *lx->s <= 'Z') || 
        (*lx->s >= 'a' && *lx->s <= 'z') || 
        *lx->s == '_') {
        lx->s++;
        while ((*lx->s >= 'A' && *lx->s <= 'Z') || 
               (*lx->s >= 'a' && *lx->s <= 'z') || 
               (*lx->s >= '0' && *lx->s <= '9') || 
               *lx->s == '_' || *lx->s == '$' || *lx->s == '%' ||
               *lx->s == '&' || *lx->s == '!' || *lx->s == '#') {
            lx->s++;
        }
        n = (int)(lx->s - start);
        if (n > maxLen - 1) n = maxLen - 1;
        _tcsncpy(out, start, n);
        out[n] = 0;
        return TRUE;
    }
    return FALSE;
}

static int OperatorPrecedence(TCHAR op) {
    switch (op) {
        case '^': return 5;
        case '*': case '/': case '\\': return 4;
        case '+': case '-': return 3;
        case '=': case '<': case '>': return 2;
        default: return 0;
    }
}

static BOOL IsRightAssociative(TCHAR op) {
    return op == '^';
}

/* Built-in function handling */
static double CallBuiltinFunction(const TCHAR *funcName, double arg) {
    TCHAR upper[32];
    _tcsncpy(upper, funcName, 31);
    upper[31] = 0;
    _tcsupr(upper);
    
    if (_tcscmp(upper, TEXT("SIN")) == 0) return sin(arg * M_PI / 180.0);
    if (_tcscmp(upper, TEXT("COS")) == 0) return cos(arg * M_PI / 180.0);
    if (_tcscmp(upper, TEXT("TAN")) == 0) return tan(arg * M_PI / 180.0);
    if (_tcscmp(upper, TEXT("ATN")) == 0) return atan(arg) * 180.0 / M_PI;
    if (_tcscmp(upper, TEXT("SQR")) == 0) return sqrt(arg);
    if (_tcscmp(upper, TEXT("ABS")) == 0) return fabs(arg);
    if (_tcscmp(upper, TEXT("INT")) == 0) return floor(arg);
    if (_tcscmp(upper, TEXT("SGN")) == 0) return (arg > 0) ? 1 : ((arg < 0) ? -1 : 0);
    if (_tcscmp(upper, TEXT("LOG")) == 0) return log(arg);
    if (_tcscmp(upper, TEXT("EXP")) == 0) return exp(arg);
    if (_tcscmp(upper, TEXT("RND")) == 0) {
        g_rndSeed = g_rndSeed * 1103515245 + 12345;
        return (double)(g_rndSeed % 32768) / 32768.0;
    }
    if (_tcscmp(upper, TEXT("LEN")) == 0) return arg; /* Placeholder */
    if (_tcscmp(upper, TEXT("VAL")) == 0) return arg;
    if (_tcscmp(upper, TEXT("ASC")) == 0) return arg;
    if (_tcscmp(upper, TEXT("TIMER")) == 0) {
        time_t now = time(NULL);
        struct tm *t = localtime(&now);
        return t->tm_hour * 3600 + t->tm_min * 60 + t->tm_sec;
    }
    
    return 0.0;
}

static double EvaluateExpression(const TCHAR *expr) {
    TCHAR ops[128];
    int ot = 0;
    double vals[256];
    int vt = 0;
    Lexer lx = {expr};
    TCHAR c;
    double num;
    TCHAR id[64];
    
    while (1) {
        LexerSkipWhitespace(&lx);
        if (!*lx.s) break;
        
        c = *lx.s;
        
        /* Number */
        if ((c >= '0' && c <= '9') || (c == '.' && lx.s[1] >= '0' && lx.s[1] <= '9')) {
            if (LexerReadNumber(&lx, &num)) {
                if (vt < 256) vals[vt++] = num;
                continue;
            }
        }
        
        /* Negative number at start or after operator */
        if (c == '-' && (vt == 0 || ot > 0)) {
            const TCHAR *next = lx.s + 1;
            while (*next == ' ') next++;
            if (*next >= '0' && *next <= '9') {
                lx.s++;
                if (LexerReadNumber(&lx, &num)) {
                    if (vt < 256) vals[vt++] = -num;
                    continue;
                }
            }
        }
        
        /* Identifier (variable or function) */
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_') {
            if (LexerReadIdent(&lx, id, 64)) {
                LexerSkipWhitespace(&lx);
                /* Check for function call */
                if (*lx.s == '(') {
                    lx.s++; /* Skip ( */
                    /* Simple: evaluate argument */
                    const TCHAR *argStart = lx.s;
                    int depth = 1;
                    while (*lx.s && depth > 0) {
                        if (*lx.s == '(') depth++;
                        else if (*lx.s == ')') depth--;
                        if (depth > 0) lx.s++;
                    }
                    TCHAR argBuf[256];
                    int argLen = (int)(lx.s - argStart);
                    if (argLen > 255) argLen = 255;
                    _tcsncpy(argBuf, argStart, argLen);
                    argBuf[argLen] = 0;
                    if (*lx.s == ')') lx.s++;
                    
                    double argVal = EvaluateExpression(argBuf);
                    if (vt < 256) vals[vt++] = CallBuiltinFunction(id, argVal);
                } else {
                    /* Variable */
                    if (vt < 256) vals[vt++] = GetNumericVariable(id);
                }
                continue;
            }
        }
        
        /* Parenthesis */
        if (c == '(') {
            if (ot < 128) ops[ot++] = c;
            lx.s++;
            continue;
        }
        
        if (c == ')') {
            lx.s++;
            while (ot > 0 && ops[ot - 1] != '(') {
                TCHAR op = ops[--ot];
                if (vt >= 2) {
                    double b = vals[--vt];
                    double a = vals[--vt];
                    switch (op) {
                        case '+': vals[vt++] = a + b; break;
                        case '-': vals[vt++] = a - b; break;
                        case '*': vals[vt++] = a * b; break;
                        case '/': vals[vt++] = (b != 0) ? a / b : 0; break;
                        case '\\': vals[vt++] = (b != 0) ? (int)a / (int)b : 0; break;
                        case '^': vals[vt++] = pow(a, b); break;
                        case '=': vals[vt++] = (a == b) ? -1 : 0; break;
                        case '<': vals[vt++] = (a < b) ? -1 : 0; break;
                        case '>': vals[vt++] = (a > b) ? -1 : 0; break;
                    }
                }
            }
            if (ot > 0 && ops[ot - 1] == '(') ot--;
            continue;
        }
        
        /* Operators */
        if (c == '+' || c == '-' || c == '*' || c == '/' || c == '\\' || 
            c == '^' || c == '=' || c == '<' || c == '>') {
            TCHAR op = c;
            lx.s++;
            
            /* Handle compound operators */
            if ((op == '<' || op == '>') && *lx.s == '=') {
                lx.s++;
                /* <= or >= - compare and push result */
            }
            if (op == '<' && *lx.s == '>') {
                lx.s++;
                op = '!'; /* <> means not equal */
            }
            
            while (ot > 0 && ops[ot - 1] != '(' &&
                   (OperatorPrecedence(ops[ot - 1]) > OperatorPrecedence(op) ||
                    (OperatorPrecedence(ops[ot - 1]) == OperatorPrecedence(op) && 
                     !IsRightAssociative(op)))) {
                TCHAR topOp = ops[--ot];
                if (vt >= 2) {
                    double b = vals[--vt];
                    double a = vals[--vt];
                    switch (topOp) {
                        case '+': vals[vt++] = a + b; break;
                        case '-': vals[vt++] = a - b; break;
                        case '*': vals[vt++] = a * b; break;
                        case '/': vals[vt++] = (b != 0) ? a / b : 0; break;
                        case '\\': vals[vt++] = (b != 0) ? (int)a / (int)b : 0; break;
                        case '^': vals[vt++] = pow(a, b); break;
                        case '=': vals[vt++] = (a == b) ? -1 : 0; break;
                        case '<': vals[vt++] = (a < b) ? -1 : 0; break;
                        case '>': vals[vt++] = (a > b) ? -1 : 0; break;
                        case '!': vals[vt++] = (a != b) ? -1 : 0; break;
                    }
                }
            }
            if (ot < 128) ops[ot++] = op;
            continue;
        }
        
        /* Unknown character - skip */
        lx.s++;
    }
    
    /* Process remaining operators */
    while (ot > 0) {
        TCHAR op = ops[--ot];
        if (vt >= 2 && op != '(') {
            double b = vals[--vt];
            double a = vals[--vt];
            switch (op) {
                case '+': vals[vt++] = a + b; break;
                case '-': vals[vt++] = a - b; break;
                case '*': vals[vt++] = a * b; break;
                case '/': vals[vt++] = (b != 0) ? a / b : 0; break;
                case '\\': vals[vt++] = (b != 0) ? (int)a / (int)b : 0; break;
                case '^': vals[vt++] = pow(a, b); break;
                case '=': vals[vt++] = (a == b) ? -1 : 0; break;
                case '<': vals[vt++] = (a < b) ? -1 : 0; break;
                case '>': vals[vt++] = (a > b) ? -1 : 0; break;
                case '!': vals[vt++] = (a != b) ? -1 : 0; break;
            }
        }
    }
    
    return (vt > 0) ? vals[vt - 1] : 0.0;
}

static BOOL EvaluateCondition(const TCHAR *cond) {
    return fabs(EvaluateExpression(cond)) > 0.0001;
}

/* ============================================================================
 * Text Interpolation (*VAR* syntax)
 * ============================================================================ */

static TCHAR *InterpolateText(const TCHAR *text, TCHAR *buffer, int bufSize) {
    const TCHAR *src = text;
    TCHAR *dst = buffer;
    TCHAR *end = buffer + bufSize - 1;
    
    while (*src && dst < end) {
        if (*src == '*') {
            const TCHAR *start = src + 1;
            const TCHAR *endStar = _tcschr(start, '*');
            if (endStar && endStar > start) {
                TCHAR varName[64];
                int len = (int)(endStar - start);
                if (len > 63) len = 63;
                _tcsncpy(varName, start, len);
                varName[len] = 0;
                
                const TCHAR *val = GetVariable(varName);
                while (*val && dst < end) {
                    *dst++ = *val++;
                }
                src = endStar + 1;
                continue;
            }
        }
        *dst++ = *src++;
    }
    *dst = 0;
    return buffer;
}

/* ============================================================================
 * Line Number Navigation
 * ============================================================================ */

static int FindLineIndex(int lineNum) {
    int i;
    for (i = 0; i < g_totalLines; i++) {
        if (g_lineNumbers[i] == lineNum) return i;
    }
    return -1;
}

/* ============================================================================
 * Command Execution
 * ============================================================================ */

static void Cmd_PRINT(const TCHAR *args) {
    TCHAR output[2048] = {0};
    TCHAR interp[2048];
    const TCHAR *p = args;
    BOOL first = TRUE;
    BOOL suppressNewline = FALSE;
    
    while (*p) {
        while (*p == ' ' || *p == '\t') p++;
        if (!*p) break;
        
        if (*p == '"') {
            /* String literal */
            p++;
            const TCHAR *start = p;
            while (*p && *p != '"') p++;
            int len = (int)(p - start);
            if (len > 0) {
                if (!first) _tcscat(output, TEXT(" "));
                _tcsncat(output, start, len);
                first = FALSE;
            }
            if (*p == '"') p++;
        } else if (*p == ';') {
            suppressNewline = TRUE;
            p++;
        } else if (*p == ',') {
            _tcscat(output, TEXT("\t"));
            p++;
        } else {
            /* Expression or variable */
            TCHAR expr[256] = {0};
            int ei = 0;
            while (*p && *p != ';' && *p != ',' && *p != '"' && ei < 255) {
                expr[ei++] = *p++;
            }
            expr[ei] = 0;
            
            /* Trim trailing whitespace */
            while (ei > 0 && (expr[ei-1] == ' ' || expr[ei-1] == '\t')) {
                expr[--ei] = 0;
            }
            
            if (ei > 0) {
                double val = EvaluateExpression(expr);
                TCHAR numBuf[64];
                
                /* Format number nicely */
                if (val == (int)val) {
                    _stprintf(numBuf, TEXT("%d"), (int)val);
                } else {
                    _stprintf(numBuf, TEXT("%g"), val);
                }
                
                if (!first) _tcscat(output, TEXT(" "));
                _tcscat(output, numBuf);
                first = FALSE;
            }
        }
    }
    
    InterpolateText(output, interp, 2048);
    
    if (suppressNewline) {
        Console_Write(interp);
    } else {
        Console_WriteLine(interp);
    }
}

static void Cmd_LET(const TCHAR *args) {
    const TCHAR *eq = _tcschr(args, '=');
    if (!eq) return;
    
    TCHAR varName[64];
    int len = (int)(eq - args);
    if (len > 63) len = 63;
    _tcsncpy(varName, args, len);
    varName[len] = 0;
    
    /* Trim whitespace */
    TCHAR *p = varName;
    while (*p == ' ') p++;
    TCHAR *e = varName + _tcslen(varName) - 1;
    while (e > varName && (*e == ' ' || *e == '\t')) *e-- = 0;
    
    const TCHAR *expr = eq + 1;
    while (*expr == ' ') expr++;
    
    if (GetVarType(p) == VAR_TYPE_STRING) {
        /* String assignment */
        TCHAR strVal[1024];
        if (*expr == '"') {
            expr++;
            const TCHAR *end = _tcschr(expr, '"');
            if (end) {
                int slen = (int)(end - expr);
                if (slen > 1023) slen = 1023;
                _tcsncpy(strVal, expr, slen);
                strVal[slen] = 0;
            } else {
                _tcscpy(strVal, expr);
            }
        } else {
            _tcscpy(strVal, expr);
        }
        SetVariable(p, strVal);
    } else {
        double val = EvaluateExpression(expr);
        SetNumericVariable(p, val);
    }
}

static void Cmd_INPUT(const TCHAR *args) {
    TCHAR prompt[256] = TEXT("? ");
    TCHAR varName[64] = {0};
    const TCHAR *p = args;
    
    /* Check for prompt string */
    if (*p == '"') {
        p++;
        const TCHAR *end = _tcschr(p, '"');
        if (end) {
            int len = (int)(end - p);
            if (len > 255) len = 255;
            _tcsncpy(prompt, p, len);
            prompt[len] = 0;
            _tcscat(prompt, TEXT(" "));
            p = end + 1;
            while (*p == ';' || *p == ',' || *p == ' ') p++;
        }
    }
    
    _tcsncpy(varName, p, 63);
    varName[63] = 0;
    
    /* Trim */
    TCHAR *e = varName + _tcslen(varName) - 1;
    while (e > varName && (*e == ' ' || *e == '\t')) *e-- = 0;
    
    Console_Write(prompt);
    
    /* For now, set to 0 - actual input requires UI integration */
    SetVariable(varName, TEXT("0"));
}

static void Cmd_IF(const TCHAR *args) {
    TCHAR upper[512];
    _tcsncpy(upper, args, 511);
    upper[511] = 0;
    _tcsupr(upper);
    
    const TCHAR *thenPos = _tcsstr(upper, TEXT("THEN"));
    if (!thenPos) return;
    
    int condLen = (int)(thenPos - upper);
    TCHAR condition[256];
    _tcsncpy(condition, args, condLen);
    condition[condLen] = 0;
    
    const TCHAR *thenPart = args + condLen + 4;
    while (*thenPart == ' ') thenPart++;
    
    if (EvaluateCondition(condition)) {
        /* Check if THEN is followed by line number */
        if (*thenPart >= '0' && *thenPart <= '9') {
            int lineNum = _ttoi(thenPart);
            int idx = FindLineIndex(lineNum);
            if (idx >= 0) {
                g_currentLine = idx - 1; /* Will be incremented */
            }
        } else {
            /* Execute statement after THEN */
            ExecuteCommand(thenPart);
        }
    }
}

static void Cmd_GOTO(const TCHAR *args) {
    int lineNum = _ttoi(args);
    int idx = FindLineIndex(lineNum);
    if (idx >= 0) {
        g_currentLine = idx - 1;
    }
}

static void Cmd_GOSUB(const TCHAR *args) {
    int lineNum = _ttoi(args);
    int idx = FindLineIndex(lineNum);
    if (idx >= 0 && g_gosubStackPtr < MAX_GOSUB_DEPTH) {
        g_gosubStack[g_gosubStackPtr++] = g_currentLine;
        g_currentLine = idx - 1;
    }
}

static void Cmd_RETURN(void) {
    if (g_gosubStackPtr > 0) {
        g_currentLine = g_gosubStack[--g_gosubStackPtr];
    }
}

static void Cmd_FOR(const TCHAR *args) {
    TCHAR upper[256];
    _tcsncpy(upper, args, 255);
    upper[255] = 0;
    _tcsupr(upper);
    
    /* Parse: var = start TO end [STEP step] */
    const TCHAR *eq = _tcschr(args, '=');
    const TCHAR *toPos = _tcsstr(upper, TEXT("TO"));
    if (!eq || !toPos) return;
    
    TCHAR varName[32];
    int vlen = (int)(eq - args);
    if (vlen > 31) vlen = 31;
    _tcsncpy(varName, args, vlen);
    varName[vlen] = 0;
    while (varName[0] == ' ') memmove(varName, varName + 1, _tcslen(varName));
    
    const TCHAR *startExpr = eq + 1;
    int startLen = (int)((args + (toPos - upper)) - startExpr);
    TCHAR startBuf[64];
    if (startLen > 63) startLen = 63;
    _tcsncpy(startBuf, startExpr, startLen);
    startBuf[startLen] = 0;
    
    const TCHAR *endExpr = args + (toPos - upper) + 2;
    while (*endExpr == ' ') endExpr++;
    
    const TCHAR *stepPos = _tcsstr(upper, TEXT("STEP"));
    double startVal = EvaluateExpression(startBuf);
    double endVal, stepVal = 1.0;
    
    if (stepPos) {
        TCHAR endBuf[64];
        int endLen = (int)((args + (stepPos - upper)) - endExpr);
        if (endLen > 63) endLen = 63;
        _tcsncpy(endBuf, endExpr, endLen);
        endBuf[endLen] = 0;
        endVal = EvaluateExpression(endBuf);
        
        const TCHAR *stepExpr = args + (stepPos - upper) + 4;
        stepVal = EvaluateExpression(stepExpr);
    } else {
        endVal = EvaluateExpression(endExpr);
    }
    
    SetNumericVariable(varName, startVal);
    
    if (g_forStackPtr < MAX_FOR_DEPTH) {
        _tcscpy(g_forStack[g_forStackPtr].varName, varName);
        g_forStack[g_forStackPtr].endValue = endVal;
        g_forStack[g_forStackPtr].step = stepVal;
        g_forStack[g_forStackPtr].forLine = g_currentLine;
        g_forStackPtr++;
    }
}

static void Cmd_NEXT(const TCHAR *args) {
    if (g_forStackPtr == 0) return;
    
    ForContext *ctx = &g_forStack[g_forStackPtr - 1];
    double current = GetNumericVariable(ctx->varName);
    double next = current + ctx->step;
    
    SetNumericVariable(ctx->varName, next);
    
    BOOL shouldContinue;
    if (ctx->step > 0) {
        shouldContinue = (next <= ctx->endValue);
    } else {
        shouldContinue = (next >= ctx->endValue);
    }
    
    if (shouldContinue) {
        g_currentLine = ctx->forLine;
    } else {
        g_forStackPtr--;
    }
}

static void Cmd_DIM(const TCHAR *args) {
    TCHAR *p = (TCHAR *)args;
    
    while (*p) {
        while (*p == ' ' || *p == ',') p++;
        if (!*p) break;
        
        TCHAR name[64];
        int ni = 0;
        while (*p && *p != '(' && ni < 63) {
            name[ni++] = *p++;
        }
        name[ni] = 0;
        
        if (*p == '(') {
            p++;
            int dims[4];
            int numDims = 0;
            
            while (*p && *p != ')' && numDims < 4) {
                TCHAR dimBuf[32];
                int di = 0;
                while (*p && *p != ',' && *p != ')' && di < 31) {
                    dimBuf[di++] = *p++;
                }
                dimBuf[di] = 0;
                dims[numDims++] = (int)EvaluateExpression(dimBuf);
                if (*p == ',') p++;
            }
            if (*p == ')') p++;
            
            CreateArray(name, dims, numDims);
        }
    }
}

static void Cmd_DATA(const TCHAR *args) {
    const TCHAR *p = args;
    
    while (*p && g_dataCount < MAX_DATA_VALUES) {
        while (*p == ' ' || *p == ',') p++;
        if (!*p) break;
        
        TCHAR value[256] = {0};
        int vi = 0;
        
        if (*p == '"') {
            p++;
            while (*p && *p != '"' && vi < 255) {
                value[vi++] = *p++;
            }
            if (*p == '"') p++;
        } else {
            while (*p && *p != ',' && vi < 255) {
                value[vi++] = *p++;
            }
        }
        value[vi] = 0;
        
        /* Trim */
        while (vi > 0 && (value[vi-1] == ' ' || value[vi-1] == '\t')) {
            value[--vi] = 0;
        }
        
        _tcscpy(g_dataValues[g_dataCount++], value);
    }
}

static void Cmd_READ(const TCHAR *args) {
    const TCHAR *p = args;
    
    while (*p) {
        while (*p == ' ' || *p == ',') p++;
        if (!*p) break;
        
        TCHAR varName[64];
        int vi = 0;
        while (*p && *p != ',' && *p != ' ' && vi < 63) {
            varName[vi++] = *p++;
        }
        varName[vi] = 0;
        
        if (g_dataPointer < g_dataCount) {
            SetVariable(varName, g_dataValues[g_dataPointer++]);
        }
    }
}

static void Cmd_RESTORE(const TCHAR *args) {
    g_dataPointer = 0;
}

static void Cmd_CLS(void) {
    if (g_hwndCanvas) {
        Canvas_Clear(g_hwndCanvas);
    }
}

static void Cmd_LINE(const TCHAR *args) {
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0;
    
    /* Parse (x1,y1)-(x2,y2) or x1,y1,x2,y2 */
    if (*args == '(') {
        _stscanf(args, TEXT("(%d,%d)-(%d,%d)"), &x1, &y1, &x2, &y2);
    } else {
        _stscanf(args, TEXT("%d,%d,%d,%d"), &x1, &y1, &x2, &y2);
    }
    
    if (g_hwndCanvas) {
        Canvas_MoveTo(g_hwndCanvas, x1, y1);
        Canvas_LineTo(g_hwndCanvas, x2, y2);
    }
}

static void Cmd_CIRCLE(const TCHAR *args) {
    int x = 0, y = 0, r = 0;
    
    if (*args == '(') {
        _stscanf(args, TEXT("(%d,%d),%d"), &x, &y, &r);
    } else {
        _stscanf(args, TEXT("%d,%d,%d"), &x, &y, &r);
    }
    
    if (g_hwndCanvas) {
        Canvas_DrawCircle(g_hwndCanvas, x, y, r);
    }
}

static void Cmd_COLOR(const TCHAR *args) {
    int fg = 7, bg = 0;
    _stscanf(args, TEXT("%d,%d"), &fg, &bg);
    /* Set colors - would need palette mapping */
}

static void Cmd_LOCATE(const TCHAR *args) {
    int row = 1, col = 1;
    _stscanf(args, TEXT("%d,%d"), &row, &col);
    g_cursorRow = row - 1;
    g_cursorCol = col - 1;
}

static void Cmd_SCREEN(const TCHAR *args) {
    int mode = _ttoi(args);
    /* Set screen mode - graphics vs text */
    if (mode > 0 && g_hwndCanvas) {
        Canvas_Clear(g_hwndCanvas);
    }
}

static void Cmd_BEEP(void) {
    MessageBeep(MB_OK);
}

static void Cmd_SOUND(const TCHAR *args) {
    int freq = 440, duration = 1;
    _stscanf(args, TEXT("%d,%d"), &freq, &duration);
    Beep(freq, duration * 55); /* Duration in clock ticks (18.2/sec) */
}

static void Cmd_END(void) {
    g_running = FALSE;
}

/* ============================================================================
 * Command Dispatcher
 * ============================================================================ */

static void ExecuteCommand(const TCHAR *line) {
    TCHAR cmd[16] = {0};
    const TCHAR *p = line;
    int ci = 0;
    
    /* Skip whitespace */
    while (*p == ' ' || *p == '\t') p++;
    if (!*p) return;
    
    /* Extract keyword */
    while (*p && !isspace((unsigned char)*p) && ci < 15) {
        cmd[ci++] = (TCHAR)toupper(*p);
        p++;
    }
    cmd[ci] = 0;
    
    /* Skip whitespace after keyword */
    while (*p == ' ' || *p == '\t') p++;
    
    /* Dispatch */
    if (_tcscmp(cmd, TEXT("PRINT")) == 0) Cmd_PRINT(p);
    else if (_tcscmp(cmd, TEXT("LET")) == 0) Cmd_LET(p);
    else if (_tcscmp(cmd, TEXT("INPUT")) == 0) Cmd_INPUT(p);
    else if (_tcscmp(cmd, TEXT("IF")) == 0) Cmd_IF(p);
    else if (_tcscmp(cmd, TEXT("GOTO")) == 0) Cmd_GOTO(p);
    else if (_tcscmp(cmd, TEXT("GOSUB")) == 0) Cmd_GOSUB(p);
    else if (_tcscmp(cmd, TEXT("RETURN")) == 0) Cmd_RETURN();
    else if (_tcscmp(cmd, TEXT("FOR")) == 0) Cmd_FOR(p);
    else if (_tcscmp(cmd, TEXT("NEXT")) == 0) Cmd_NEXT(p);
    else if (_tcscmp(cmd, TEXT("DIM")) == 0) Cmd_DIM(p);
    else if (_tcscmp(cmd, TEXT("DATA")) == 0) Cmd_DATA(p);
    else if (_tcscmp(cmd, TEXT("READ")) == 0) Cmd_READ(p);
    else if (_tcscmp(cmd, TEXT("RESTORE")) == 0) Cmd_RESTORE(p);
    else if (_tcscmp(cmd, TEXT("CLS")) == 0) Cmd_CLS();
    else if (_tcscmp(cmd, TEXT("LINE")) == 0) Cmd_LINE(p);
    else if (_tcscmp(cmd, TEXT("CIRCLE")) == 0) Cmd_CIRCLE(p);
    else if (_tcscmp(cmd, TEXT("COLOR")) == 0) Cmd_COLOR(p);
    else if (_tcscmp(cmd, TEXT("LOCATE")) == 0) Cmd_LOCATE(p);
    else if (_tcscmp(cmd, TEXT("SCREEN")) == 0) Cmd_SCREEN(p);
    else if (_tcscmp(cmd, TEXT("BEEP")) == 0) Cmd_BEEP();
    else if (_tcscmp(cmd, TEXT("SOUND")) == 0) Cmd_SOUND(p);
    else if (_tcscmp(cmd, TEXT("END")) == 0) Cmd_END();
    else if (_tcscmp(cmd, TEXT("REM")) == 0) { /* Comment */ }
    else if (_tcschr(line, '=') != NULL) {
        /* Implicit LET */
        Cmd_LET(line);
    }
}

/* ============================================================================
 * Main Execution
 * ============================================================================ */

BOOL BasicInterpreter_Execute(const TCHAR *code, HWND hwndConsole, 
                               HWND hwndCanvas, BOOL debugMode) {
    TCHAR *text;
    TCHAR *save = NULL;
    TCHAR *tok;
    
    g_hwndConsole = hwndConsole;
    g_hwndCanvas = hwndCanvas;
    g_debugMode = debugMode;
    g_running = TRUE;
    
    /* Parse program into lines */
    text = _tcsdup(code);
    tok = _tcstok_s(text, TEXT("\n"), &save);
    g_totalLines = 0;
    
    while (tok && g_totalLines < MAX_LINES) {
        TCHAR *p = tok;
        while (*p == ' ' || *p == '\t') p++;
        
        /* Check for line number */
        g_lineNumbers[g_totalLines] = -1;
        if (*p >= '0' && *p <= '9') {
            g_lineNumbers[g_totalLines] = _ttoi(p);
            while (*p >= '0' && *p <= '9') p++;
            while (*p == ' ' || *p == '\t') p++;
        }
        
        g_programLines[g_totalLines] = _tcsdup(p);
        g_totalLines++;
        
        /* Collect DATA statements */
        TCHAR upper[16];
        _tcsncpy(upper, p, 15);
        upper[15] = 0;
        _tcsupr(upper);
        if (_tcsncmp(upper, TEXT("DATA "), 5) == 0) {
            Cmd_DATA(p + 5);
        }
        
        tok = _tcstok_s(NULL, TEXT("\n"), &save);
    }
    free(text);
    
    /* Execute program */
    g_currentLine = 0;
    while (g_running && g_currentLine < g_totalLines) {
        const TCHAR *line = g_programLines[g_currentLine];
        
        if (line && *line) {
            ExecuteCommand(line);
        }
        
        g_currentLine++;
    }
    
    g_running = FALSE;
    return TRUE;
}

void BasicInterpreter_Stop(void) {
    g_running = FALSE;
}

BOOL BasicInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) {
    const TCHAR *val = GetVariable(name);
    _tcsncpy(value, val, maxLen - 1);
    value[maxLen - 1] = 0;
    return TRUE;
}

BOOL BasicInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) {
    SetVariable(name, value);
    return TRUE;
}

int BasicInterpreter_GetLineCount(void) {
    return g_totalLines;
}

const TCHAR *BasicInterpreter_GetLine(int index) {
    if (index >= 0 && index < g_totalLines) {
        return g_programLines[index];
    }
    return TEXT("");
}
