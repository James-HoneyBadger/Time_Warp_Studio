/*
 * basic_interpreter.c - BASIC interpreter
 */

#include "basic_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <tchar.h>
#include "canvas.h"

#ifndef _MSC_VER
/* Guard fallback mappings only if not provided by toolchain */
#ifndef _tcstok_s
#define _tcstok_s(str, delim, ctx) _tcstok(str, delim)
#endif
#ifndef _tcsdup
#define _tcsdup _strdup
#endif
/* _tcsnicmp and _tstof are available in MinGW, avoid redefining */
#endif

typedef struct
{
    TCHAR name[32];
    double value;
} Variable;

static Variable g_variables[256];
static int g_varCount = 0;
static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static HWND g_hwndCanvas = NULL;
static int g_currentLine = 0;
static int g_totalLines = 0;
static int g_lineNumbers[2048]; // map index->line number if present, else -1

// GOSUB/RETURN stack
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

static void Basic_Print(const TCHAR *text)
{
    if (g_hwndConsole)
    {
        TCHAR buf[1024];
        _stprintf(buf, TEXT("%s\r\n"), text);
        int len = GetWindowTextLength(g_hwndConsole);
        SendMessage(g_hwndConsole, EM_SETSEL, len, len);
        SendMessage(g_hwndConsole, EM_REPLACESEL, FALSE, (LPARAM)buf);
    }
}

static void Console_Write(const TCHAR *text)
{
    if (g_hwndConsole)
    {
        int len = GetWindowTextLength(g_hwndConsole);
        SendMessage(g_hwndConsole, EM_SETSEL, len, len);
        SendMessage(g_hwndConsole, EM_REPLACESEL, FALSE, (LPARAM)text);
    }
}

// -------- Expression parser (shunting-yard) ---------
typedef struct
{
    const TCHAR *s;
} Lexer;

static void lx_skip_ws(Lexer *lx)
{
    while (*lx->s == TEXT(' ') || *lx->s == TEXT('\t'))
        lx->s++;
}
static BOOL lx_peek(Lexer *lx, TCHAR *ch)
{
    lx_skip_ws(lx);
    if (*lx->s)
    {
        *ch = *lx->s;
        return TRUE;
    }
    return FALSE;
}
static BOOL lx_read_number(Lexer *lx, double *out)
{
    lx_skip_ws(lx);
    const TCHAR *start = lx->s;
    BOOL dot = FALSE;
    if (*lx->s == TEXT('-'))
        lx->s++;
    while ((*lx->s >= TEXT('0') && *lx->s <= TEXT('9')) || *lx->s == TEXT('.'))
    {
        if (*lx->s == TEXT('.'))
        {
            if (dot)
                break;
            dot = TRUE;
        }
        lx->s++;
    }
    if (lx->s > start)
    {
        TCHAR buf[64];
        int n = (int)(lx->s - start);
        n = min(n, 63);
        _tcsncpy(buf, start, n);
        buf[n] = 0;
        *out = _tstof(buf);
        return TRUE;
    }
    return FALSE;
}
static BOOL lx_read_ident(Lexer *lx, TCHAR *out, int max)
{
    lx_skip_ws(lx);
    const TCHAR *start = lx->s;
    if (((*lx->s >= 'A' && *lx->s <= 'Z') || (*lx->s >= 'a' && *lx->s <= 'z') || *lx->s == TEXT('_')))
    {
        lx->s++;
        while (((*lx->s >= 'A' && *lx->s <= 'Z') || (*lx->s >= 'a' && *lx->s <= 'z') || (*lx->s >= '0' && *lx->s <= '9') || *lx->s == TEXT('_')))
            lx->s++;
        int n = (int)(lx->s - start);
        n = min(n, max - 1);
        _tcsncpy(out, start, n);
        out[n] = 0;
        return TRUE;
    }
    return FALSE;
}

static double get_var(const TCHAR *name)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (_tcsicmp(g_variables[i].name, name) == 0)
            return g_variables[i].value;
    }
    return 0.0;
}
static void set_var(const TCHAR *name, double v)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (_tcsicmp(g_variables[i].name, name) == 0)
        {
            g_variables[i].value = v;
            return;
        }
    }
    if (g_varCount < 256)
    {
        _tcsncpy(g_variables[g_varCount].name, name, 31);
        g_variables[g_varCount].name[31] = 0;
        g_variables[g_varCount].value = v;
        g_varCount++;
    }
}

static int op_prec(TCHAR op)
{
    switch (op)
    {
    case '^':
        return 4;
    case '*':
    case '/':
        return 3;
    case '+':
    case '-':
        return 2;
    default:
        return 0;
    }
}
static BOOL op_right_assoc(TCHAR op) { return op == '^'; }

static double eval_expr(const TCHAR *expr)
{
    // Convert to RPN using shunting-yard and evaluate
    TCHAR ops[128];
    int ot = 0;
    double vals[256];
    int vt = 0;
    Lexer lx = {expr};
    while (1)
    {
        lx_skip_ws(&lx);
        if (!*lx.s)
            break;
        TCHAR c = *lx.s;
        if ((c >= '0' && c <= '9') || c == TEXT('-'))
        {
            double num;
            if (lx_read_number(&lx, &num))
            {
                vals[vt++] = num;
                continue;
            }
        }
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == TEXT('_'))
        {
            TCHAR id[64];
            if (lx_read_ident(&lx, id, 64))
            {
                vals[vt++] = get_var(id);
                continue;
            }
        }
        if (c == TEXT('('))
        {
            ops[ot++] = c;
            lx.s++;
            continue;
        }
        if (c == TEXT(')'))
        {
            lx.s++;
            while (ot > 0 && ops[ot - 1] != TEXT('('))
            {
                TCHAR op = ops[--ot];
                double b = vals[--vt], a = vals[--vt];
                switch (op)
                {
                case '+':
                    vals[vt++] = a + b;
                    break;
                case '-':
                    vals[vt++] = a - b;
                    break;
                case '*':
                    vals[vt++] = a * b;
                    break;
                case '/':
                    vals[vt++] = b != 0 ? a / b : 0;
                    break;
                case '^':
                    vals[vt++] = pow(a, b);
                    break;
                }
            }
            if (ot > 0 && ops[ot - 1] == TEXT('('))
                ot--;
            continue;
        }
        // operator
        TCHAR op = c;
        lx.s++;
        while (ot > 0 && ops[ot - 1] != TEXT('(') && (op_prec(ops[ot - 1]) > op_prec(op) || (op_prec(ops[ot - 1]) == op_prec(op) && !op_right_assoc(op))))
        {
            TCHAR top = ops[--ot];
            double b = vals[--vt], a = vals[--vt];
            switch (top)
            {
            case '+':
                vals[vt++] = a + b;
                break;
            case '-':
                vals[vt++] = a - b;
                break;
            case '*':
                vals[vt++] = a * b;
                break;
            case '/':
                vals[vt++] = b != 0 ? a / b : 0;
                break;
            case '^':
                vals[vt++] = pow(a, b);
                break;
            }
        }
        ops[ot++] = op;
    }
    while (ot > 0)
    {
        TCHAR op = ops[--ot];
        double b = vals[--vt], a = vals[--vt];
        switch (op)
        {
        case '+':
            vals[vt++] = a + b;
            break;
        case '-':
            vals[vt++] = a - b;
            break;
        case '*':
            vals[vt++] = a * b;
            break;
        case '/':
            vals[vt++] = b != 0 ? a / b : 0;
            break;
        case '^':
            vals[vt++] = pow(a, b);
            break;
        }
    }
    return vt > 0 ? vals[vt - 1] : 0.0;
}

// ------------- Helpers -------------
static void draw_line_cmd(const TCHAR *args)
{
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0;
    _stscanf(args, TEXT("%d , %d , %d , %d"), &x1, &y1, &x2, &y2);
    if (g_hwndCanvas)
    {
        Canvas_MoveTo(g_hwndCanvas, x1, y1);
        Canvas_LineTo(g_hwndCanvas, x2, y2);
    }
}
static void circle_cmd(const TCHAR *args)
{
    int x = 0, y = 0, r = 0;
    _stscanf(args, TEXT("%d , %d , %d"), &x, &y, &r);
    if (g_hwndCanvas)
    {
        Canvas_DrawCircle(g_hwndCanvas, x, y, r);
    }
}

static int find_line_index_by_number(int target)
{
    for (int i = 0; i < g_totalLines; i++)
    {
        if (g_lineNumbers[i] == target)
            return i;
    }
    return -1;
}

BOOL BasicInterpreter_Execute(const TCHAR *code, HWND hwndConsole, HWND hwndCanvas, BOOL debugMode)
{
    g_hwndConsole = hwndConsole;
    g_hwndCanvas = hwndCanvas;
    g_running = TRUE;
    // Split into lines and parse optional line numbers
    TCHAR *text = _tcsdup(code);
    TCHAR *save = NULL;
    TCHAR *tok = _tcstok_s(text, TEXT("\n"), &save);
    TCHAR *lines[2048];
    g_totalLines = 0;
    while (tok && g_totalLines < 2048)
    {
        lines[g_totalLines] = tok;
        g_lineNumbers[g_totalLines] = -1; // detect leading number
        TCHAR tmp[16];
        int num = 0;
        if (_stscanf(tok, TEXT("%d %15s"), &num, tmp) == 2)
        {
            g_lineNumbers[g_totalLines] = num;
        }
        g_totalLines++;
        tok = _tcstok_s(NULL, TEXT("\n"), &save);
    }
    g_currentLine = 0;
    while (g_running && g_currentLine < g_totalLines)
    {
        TCHAR *ln = lines[g_currentLine];
        // Skip optional line number token
        while (*ln == TEXT(' ') || *ln == TEXT('\t'))
            ln++;
        if (*ln && (*ln >= TEXT('0') && *ln <= TEXT('9')))
        {
            while (*ln && *ln != TEXT(' ') && *ln != TEXT('\t'))
                ln++;
            while (*ln == TEXT(' ') || *ln == TEXT('\t'))
                ln++;
        }
        if (*ln == 0)
        {
            g_currentLine++;
            continue;
        }
        // Keyword and rest
        TCHAR kw[16] = {0};
        int pos = 0;
        const TCHAR *p = ln;
        while (*p && !isspace((unsigned char)*p) && pos < 15)
        {
            kw[pos++] = *p++;
        }
        kw[pos] = 0;
        while (*p == TEXT(' ') || *p == TEXT('\t'))
            p++;
        if (_tcsicmp(kw, TEXT("PRINT")) == 0)
        {
            // Support expressions separated by commas
            TCHAR buf[1024] = {0};
            const TCHAR *a = p;
            int first = 1;
            while (*a)
            {
                // Split on commas not inside quotes (no quotes support here yet)
                const TCHAR *b = _tcschr(a, TEXT(','));
                TCHAR part[256];
                if (b)
                {
                    int n = (int)(b - a);
                    n = min(n, 255);
                    _tcsncpy(part, a, n);
                    part[n] = 0;
                    a = b + 1;
                }
                else
                {
                    _tcsncpy(part, a, 255);
                    part[255] = 0;
                    a += _tcslen(a);
                }
                double v = eval_expr(part);
                TCHAR tmp[64];
                _stprintf(tmp, TEXT("%s%g"), first ? TEXT("") : TEXT(" "), v);
                _tcscat(buf, tmp);
                first = 0;
                if (!b)
                    break;
            }
            if (first)
            { // no numbers parsed, print raw
                Basic_Print(p);
            }
            else
            {
                Basic_Print(buf);
            }
            g_currentLine++;
        }
        else if (_tcsicmp(kw, TEXT("LET")) == 0)
        {
            // LET var = expr
            TCHAR var[32] = {0};
            const TCHAR *eq = _tcschr(p, TEXT('='));
            if (eq)
            {
                int n = (int)(eq - p);
                n = min(n, 31);
                _tcsncpy(var, p, n);
                var[n] = 0;
                const TCHAR *ex = eq + 1;
                double v = eval_expr(ex);
                set_var(var, v);
            }
            g_currentLine++;
        }
        else if (_tcsicmp(kw, TEXT("IF")) == 0)
        {
            // IF expr THEN target
            const TCHAR *then = _tcsstr(p, TEXT("THEN"));
            if (then)
            {
                TCHAR cond[256];
                int n = (int)(then - p);
                n = min(n, 255);
                _tcsncpy(cond, p, n);
                cond[n] = 0;
                const TCHAR *t = then + 4;
                while (*t == TEXT(' ') || *t == TEXT('\t'))
                    t++;
                double cv = eval_expr(cond);
                if (cv != 0)
                {
                    if (*t >= TEXT('0') && *t <= TEXT('9'))
                    {
                        int lno = _ttoi(t);
                        int idx = find_line_index_by_number(lno);
                        g_currentLine = (idx >= 0 ? idx : g_currentLine + 1);
                    }
                    else
                    { // single PRINT etc.
                        // naive: support GOTO
                        if (_tcsnicmp(t, TEXT("GOTO"), 4) == 0)
                        {
                            int lno = _ttoi(t + 4);
                            int idx = find_line_index_by_number(lno);
                            g_currentLine = (idx >= 0 ? idx : g_currentLine + 1);
                        }
                        else
                        {
                            g_currentLine++;
                        }
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
        else if (_tcsicmp(kw, TEXT("GOTO")) == 0)
        {
            int lno = _ttoi(p);
            int idx = find_line_index_by_number(lno);
            g_currentLine = (idx >= 0 ? idx : g_currentLine + 1);
        }
        else if (_tcsicmp(kw, TEXT("INPUT")) == 0)
        {
            // INPUT var or INPUT "prompt"; var
            // Simplified: just prompt in console and read one line
            TCHAR prompt[256] = TEXT("? ");
            TCHAR varname[32] = {0};
            const TCHAR *semi = _tcschr(p, TEXT(';'));
            if (semi)
            {
                // Extract prompt before semicolon
                int n = (int)(semi - p);
                n = min(n, 255);
                _tcsncpy(prompt, p, n);
                prompt[n] = 0;
                // Trim quotes if present
                TCHAR *q = prompt;
                while (*q == TEXT(' ') || *q == TEXT('\t') || *q == TEXT('"'))
                    q++;
                TCHAR *e = q + _tcslen(q) - 1;
                while (e > q && (*e == TEXT(' ') || *e == TEXT('\t') || *e == TEXT('"')))
                    *e-- = 0;
                _tcscpy(prompt, q);
                _tcscat(prompt, TEXT(" "));

                // Variable name after semicolon
                const TCHAR *v = semi + 1;
                while (*v == TEXT(' ') || *v == TEXT('\t'))
                    v++;
                _tcsncpy(varname, v, 31);
                varname[31] = 0;
            }
            else
            {
                // Just variable name
                _tcsncpy(varname, p, 31);
                varname[31] = 0;
            }
            // Display prompt
            Console_Write(prompt);
            // Note: Actual input would require message pump integration
            // For now, just set variable to 0 (stub)
            set_var(varname, 0.0);
            g_currentLine++;
        }
        else if (_tcsicmp(kw, TEXT("GOSUB")) == 0)
        {
            int lno = _ttoi(p);
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
        else if (_tcsicmp(kw, TEXT("RETURN")) == 0)
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
        else if (_tcsicmp(kw, TEXT("FOR")) == 0)
        {
            // Minimal FOR not fully implemented: skip for now
            g_currentLine++;
        }
        else if (_tcsicmp(kw, TEXT("CLS")) == 0)
        {
            if (g_hwndCanvas)
                Canvas_Clear(g_hwndCanvas);
            g_currentLine++;
        }
        else if (_tcsicmp(kw, TEXT("LINE")) == 0)
        {
            draw_line_cmd(p);
            g_currentLine++;
        }
        else if (_tcsicmp(kw, TEXT("CIRCLE")) == 0)
        {
            circle_cmd(p);
            g_currentLine++;
        }
        else if (_tcsicmp(kw, TEXT("END")) == 0)
        {
            break;
        }
        else if (_tcsicmp(kw, TEXT("REM")) == 0)
        {
            g_currentLine++;
        }
        else
        {
            g_currentLine++;
        }
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

BOOL BasicInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (_tcsicmp(g_variables[i].name, name) == 0)
        {
            _stprintf(value, TEXT("%g"), g_variables[i].value);
            return TRUE;
        }
    }
    return FALSE;
}

BOOL BasicInterpreter_SetVariable(const TCHAR *name, const TCHAR *value)
{
    for (int i = 0; i < g_varCount; i++)
    {
        if (_tcsicmp(g_variables[i].name, name) == 0)
        {
            g_variables[i].value = _tstof(value);
            return TRUE;
        }
    }
    if (g_varCount < 256)
    {
        _tcscpy(g_variables[g_varCount].name, name);
        g_variables[g_varCount].value = _tstof(value);
        g_varCount++;
        return TRUE;
    }
    return FALSE;
}

int BasicInterpreter_GetLineCount(void) { return g_totalLines; }
const TCHAR *BasicInterpreter_GetLine(int lineNum) { return TEXT(""); }
