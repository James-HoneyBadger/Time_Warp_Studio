/*
 * pilot_interpreter.c - PILOT interpreter with full command support
 */

#include "pilot_interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <tchar.h>

#ifndef _MSC_VER
#ifndef _tcstok_s
#define _tcstok_s(str, delim, ctx) _tcstok(str, delim)
#endif
#ifndef _tcsdup
#define _tcsdup _strdup
#endif
/* Do not redefine _tcsnicmp/_tstof to prevent warnings */
#endif

typedef struct {
    TCHAR name[32];
    TCHAR value[256];
    BOOL isNumeric;
    double numValue;
} PilotVar;

typedef struct {
    TCHAR label[64];
    int lineIndex;
} PilotLabel;

static BOOL g_running = FALSE;
static HWND g_hwndConsole = NULL;
static PilotVar g_vars[128];
static int g_varCount = 0;
static PilotLabel g_labels[256];
static int g_labelCount = 0;
static TCHAR g_lastInput[256] = {0};
static BOOL g_matchFlag = FALSE;
static BOOL g_conditionResult = FALSE;

static void set_var(const TCHAR* name, const TCHAR* val){
    int idx = -1;
    for(int i=0;i<g_varCount;i++){ if(_tcsicmp(g_vars[i].name,name)==0){ idx=i; break; } }
    if(idx<0 && g_varCount<128){ idx=g_varCount++; }
    if(idx>=0){ 
        _tcsncpy(g_vars[idx].name, name, 31); g_vars[idx].name[31]=0;
        _tcsncpy(g_vars[idx].value, val, 255); g_vars[idx].value[255]=0;
        TCHAR *end; g_vars[idx].numValue = _tcstod(val, &end);
        g_vars[idx].isNumeric = (end != val && *end == 0);
    }
}

static const TCHAR* get_var(const TCHAR* name){
    for(int i=0;i<g_varCount;i++){ if(_tcsicmp(g_vars[i].name,name)==0) return g_vars[i].value; }
    return TEXT("");
}

static double get_var_num(const TCHAR* name){
    for(int i=0;i<g_varCount;i++){ if(_tcsicmp(g_vars[i].name,name)==0 && g_vars[i].isNumeric) return g_vars[i].numValue; }
    return 0.0;
}

static void register_label(const TCHAR* label, int idx){
    if(g_labelCount<256){ _tcsncpy(g_labels[g_labelCount].label, label, 63); g_labels[g_labelCount].label[63]=0; g_labels[g_labelCount].lineIndex = idx; g_labelCount++; }
}

static int find_label(const TCHAR* label){
    for(int i=0;i<g_labelCount;i++){ if(_tcsicmp(g_labels[i].label, label)==0) return g_labels[i].lineIndex; }
    return -1;
}

BOOL PilotInterpreter_Init(void) {
    g_running = FALSE;
    g_varCount = 0;
    g_labelCount = 0;
    g_matchFlag = FALSE;
    g_conditionResult = FALSE;
    g_lastInput[0] = 0;
    return TRUE;
}

void PilotInterpreter_Cleanup(void) { }

static void Pilot_Type(const TCHAR *text) {
    if (g_hwndConsole) {
        TCHAR buf[1024];
        _stprintf(buf, TEXT("%s\r\n"), text);
        int len = GetWindowTextLength(g_hwndConsole);
        SendMessage(g_hwndConsole, EM_SETSEL, len, len);
        SendMessage(g_hwndConsole, EM_REPLACESEL, FALSE, (LPARAM)buf);
    }
}

static BOOL eval_condition(const TCHAR* cond){
    TCHAR left[64]={0}, op[4]={0}, right[64]={0};
    const TCHAR* p = cond;
    int i=0; while(*p && !(_tcschr(TEXT("=<>"),*p)) && i<63){ left[i++]=*p++; }
    left[i]=0; i=0; while(*p && (_tcschr(TEXT("=<>"),*p)) && i<3){ op[i++]=*p++; }
    op[i]=0; i=0; while(*p && i<63){ right[i++]=*p++; }
    right[i]=0;
    double lv = get_var_num(left), rv = _tstof(right);
    if(_tcscmp(op,TEXT("="))==0) return (lv==rv);
    if(_tcscmp(op,TEXT(">"))==0) return (lv>rv);
    if(_tcscmp(op,TEXT("<"))==0) return (lv<rv);
    if(_tcscmp(op,TEXT(">="))==0) return (lv>=rv);
    if(_tcscmp(op,TEXT("<="))==0) return (lv<=rv);
    if(_tcscmp(op,TEXT("<>"))==0) return (lv!=rv);
    return FALSE;
}

BOOL PilotInterpreter_Execute(const TCHAR *code, HWND hwndConsole, HWND hwndCanvas, BOOL debugMode) {
    g_hwndConsole = hwndConsole;
    g_running = TRUE;
    TCHAR *dup = _tcsdup(code);
    TCHAR *save = NULL;
    TCHAR *lines[2048]; int lineCount=0;
    TCHAR *ln = _tcstok_s(dup, TEXT("\n"), &save);
    while(ln && lineCount<2048){ lines[lineCount++]=ln; ln=_tcstok_s(NULL,TEXT("\n"),&save); }
    
    for(int i=0;i<lineCount;i++){
        TCHAR *l = lines[i]; while(*l==TEXT(' ')||*l==TEXT('\t')) l++;
        if(_tcsnicmp(l,TEXT("L:"),2)==0){ register_label(l+2, i); }
    }
    
    for(int i=0;i<lineCount && g_running;i++){
        TCHAR *l = lines[i]; while(*l==TEXT(' ')||*l==TEXT('\t')) l++;
        if(_tcslen(l)<2) continue;
        TCHAR cmd = l[0];
        if(l[1]!=TEXT(':')) continue;
        const TCHAR *arg = l+2;
        
        if(cmd==TEXT('T') || cmd==TEXT('t')){
            if(g_matchFlag){ g_matchFlag=FALSE; } else { Pilot_Type(arg); }
        } else if(cmd==TEXT('A') || cmd==TEXT('a')){
            TCHAR prompt[128]; _stprintf(prompt, TEXT("%s? "), arg);
            Pilot_Type(prompt);
            _tcsncpy(g_lastInput, TEXT("userInput"), 255);
            set_var(arg, g_lastInput);
        } else if(cmd==TEXT('U') || cmd==TEXT('u')){
            const TCHAR *eq = _tcschr(arg, TEXT('='));
            if(eq){ TCHAR var[64]={0}; int n=(int)(eq-arg); n=(n<63?n:63); _tcsncpy(var,arg,n); set_var(var, eq+1); }
        } else if(cmd==TEXT('C') || cmd==TEXT('c')){
            g_conditionResult = eval_condition(arg);
        } else if(cmd==TEXT('Y') || cmd==TEXT('y')){
            BOOL test = (_tcslen(arg)>0) ? eval_condition(arg) : g_conditionResult;
            g_matchFlag = !test;
        } else if(cmd==TEXT('N') || cmd==TEXT('n')){
            BOOL test = (_tcslen(arg)>0) ? eval_condition(arg) : g_conditionResult;
            g_matchFlag = test;
        } else if(cmd==TEXT('M') || cmd==TEXT('m')){
            TCHAR pat[128]; _tcsncpy(pat, arg, 127); pat[127]=0;
            CharUpperBuff(pat, _tcslen(pat));
            TCHAR inp[256]; _tcsncpy(inp, g_lastInput, 255); inp[255]=0;
            CharUpperBuff(inp, _tcslen(inp));
            g_matchFlag = (_tcsstr(inp, pat) == NULL);
        } else if(cmd==TEXT('J') || cmd==TEXT('j')){
            int idx = find_label(arg);
            if(idx>=0){ i = idx - 1; }
        } else if(cmd==TEXT('L') || cmd==TEXT('l')){
            // Label
        } else if(cmd==TEXT('E') || cmd==TEXT('e')){
            break;
        } else if(cmd==TEXT('R') || cmd==TEXT('r')){
            Pilot_Type(TEXT("Runtime command not implemented"));
        }
    }
    free(dup);
    g_running = FALSE;
    return TRUE;
}

void PilotInterpreter_Stop(void) {
    g_running = FALSE;
}

void PilotInterpreter_Reset(void) { }
BOOL PilotInterpreter_GetVariable(const TCHAR *name, TCHAR *value, int maxLen) { return FALSE; }
BOOL PilotInterpreter_SetVariable(const TCHAR *name, const TCHAR *value) { return FALSE; }
