/*
 * editor.c - Syntax-highlighting code editor implementation
 * Full-featured editor with line numbers, bookmarks, find/replace
 */

#include "editor.h"
#include <richedit.h>
#include <commctrl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tchar.h>

#define MAX_BOOKMARKS 256
#define EDITOR_CLASS TEXT("TimeWarpEditor")

/* Editor data structure */
typedef struct
{
    HWND hwndEdit;
    HWND hwndLineNumbers;
    EditorLanguage language;
    BOOL bookmarks[MAX_BOOKMARKS];
    int bookmarkCount;
    BOOL modified;
    HFONT hFont;
    COLORREF bgColor;
    COLORREF fgColor;
} EditorData;

/* Syntax highlighting colors */
static COLORREF g_colorKeyword = RGB(0, 0, 255);
static COLORREF g_colorString = RGB(163, 21, 21);
static COLORREF g_colorComment = RGB(0, 128, 0);
static COLORREF g_colorNumber = RGB(255, 0, 255);

/* BASIC keywords */
static const TCHAR *g_basicKeywords[] = {
    TEXT("PRINT"), TEXT("LET"), TEXT("IF"), TEXT("THEN"), TEXT("ELSE"),
    TEXT("FOR"), TEXT("TO"), TEXT("STEP"), TEXT("NEXT"), TEXT("WHILE"),
    TEXT("WEND"), TEXT("DO"), TEXT("LOOP"), TEXT("UNTIL"), TEXT("GOTO"),
    TEXT("GOSUB"), TEXT("RETURN"), TEXT("INPUT"), TEXT("READ"), TEXT("DATA"),
    TEXT("DIM"), TEXT("REM"), TEXT("END"), TEXT("CLS"), TEXT("LOCATE"),
    TEXT("COLOR"), TEXT("LINE"), TEXT("CIRCLE"), TEXT("PSET"), TEXT("PRESET"),
    TEXT("ABS"), TEXT("SIN"), TEXT("COS"), TEXT("TAN"), TEXT("ATN"),
    TEXT("SQR"), TEXT("INT"), TEXT("RND"), TEXT("LEN"), TEXT("MID"),
    TEXT("LEFT"), TEXT("RIGHT"), TEXT("CHR"), TEXT("ASC"), TEXT("STR"),
    TEXT("VAL"), TEXT("AND"), TEXT("OR"), TEXT("NOT"), TEXT("XOR"),
    NULL};

/* PILOT keywords */
static const TCHAR *g_pilotKeywords[] = {
    TEXT("T:"), TEXT("A:"), TEXT("M:"), TEXT("C:"), TEXT("U:"),
    TEXT("J:"), TEXT("E:"), TEXT("Y:"), TEXT("N:"), TEXT("END"),
    NULL};

/* Logo keywords */
static const TCHAR *g_logoKeywords[] = {
    TEXT("FORWARD"), TEXT("FD"), TEXT("BACK"), TEXT("BK"), TEXT("LEFT"),
    TEXT("LT"), TEXT("RIGHT"), TEXT("RT"), TEXT("PENUP"), TEXT("PU"),
    TEXT("PENDOWN"), TEXT("PD"), TEXT("SETPENCOLOR"), TEXT("SETPC"),
    TEXT("SETPENSIZE"), TEXT("HOME"), TEXT("CLEARSCREEN"), TEXT("CS"),
    TEXT("HIDETURTLE"), TEXT("HT"), TEXT("SHOWTURTLE"), TEXT("ST"),
    TEXT("REPEAT"), TEXT("TO"), TEXT("END"), TEXT("IF"), TEXT("IFELSE"),
    TEXT("MAKE"), TEXT("PRINT"), TEXT("OUTPUT"), TEXT("STOP"),
    NULL};

/* Window procedure */
static LRESULT CALLBACK EditorWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

/* Helper functions */
static void Editor_ApplySyntaxHighlighting(HWND hwnd);
static void Editor_UpdateLineNumbers(HWND hwnd);
static BOOL Editor_IsKeyword(const TCHAR *word, EditorLanguage lang);

/*
 * Editor_Create - Create new editor window
 */
HWND Editor_Create(HWND hwndParent, HINSTANCE hInstance)
{
    /* Register editor class if not already registered */
    static BOOL registered = FALSE;
    if (!registered)
    {
        WNDCLASSEX wc = {0};
        wc.cbSize = sizeof(WNDCLASSEX);
        wc.style = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc = EditorWndProc;
        wc.hInstance = hInstance;
        wc.hCursor = LoadCursor(NULL, IDC_ARROW);
        wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
        wc.lpszClassName = EDITOR_CLASS;
        RegisterClassEx(&wc);
        registered = TRUE;
    }

    /* Create editor container */
    HWND hwnd = CreateWindowEx(
        0,
        EDITOR_CLASS,
        TEXT(""),
        WS_CHILD | WS_VISIBLE,
        0, 0, 0, 0,
        hwndParent,
        NULL,
        hInstance,
        NULL);

    if (!hwnd)
        return NULL;

    /* Allocate editor data */
    EditorData *data = (EditorData *)calloc(1, sizeof(EditorData));
    if (!data)
    {
        DestroyWindow(hwnd);
        return NULL;
    }

    /* Create RichEdit control */
    data->hwndEdit = CreateWindowEx(
        0,
        RICHEDIT_CLASS,
        TEXT(""),
        WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_HSCROLL |
            ES_MULTILINE | ES_AUTOVSCROLL | ES_AUTOHSCROLL |
            ES_NOHIDESEL | ES_WANTRETURN,
        40, 0, 0, 0,
        hwnd,
        NULL,
        hInstance,
        NULL);

    /* Create line numbers control */
    data->hwndLineNumbers = CreateWindowEx(
        0,
        TEXT("STATIC"),
        TEXT(""),
        WS_CHILD | WS_VISIBLE | SS_RIGHT,
        0, 0, 40, 0,
        hwnd,
        NULL,
        hInstance,
        NULL);

    /* Set font */
    data->hFont = CreateFont(
        -14, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE,
        DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
        DEFAULT_QUALITY, FIXED_PITCH | FF_MODERN,
        TEXT("Courier New"));

    SendMessage(data->hwndEdit, WM_SETFONT, (WPARAM)data->hFont, TRUE);
    SendMessage(data->hwndLineNumbers, WM_SETFONT, (WPARAM)data->hFont, TRUE);

    /* Initialize */
    data->language = EDITOR_LANG_BASIC;
    data->modified = FALSE;
    data->bookmarkCount = 0;
    data->bgColor = RGB(255, 255, 255);
    data->fgColor = RGB(0, 0, 0);
    memset(data->bookmarks, 0, sizeof(data->bookmarks));

    /* Set background color */
    SendMessage(data->hwndEdit, EM_SETBKGNDCOLOR, 0, data->bgColor);

    /* Store data pointer */
    SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)data);

    return hwnd;
}

/*
 * EditorWndProc - Editor window message handler
 */
static LRESULT CALLBACK EditorWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);

    switch (msg)
    {
    case WM_SIZE:
    {
        if (data && data->hwndEdit && data->hwndLineNumbers)
        {
            RECT rc;
            GetClientRect(hwnd, &rc);
            MoveWindow(data->hwndLineNumbers, 0, 0, 40, rc.bottom, TRUE);
            MoveWindow(data->hwndEdit, 40, 0, rc.right - 40, rc.bottom, TRUE);
        }
        return 0;
    }

    case WM_COMMAND:
        if (data && HIWORD(wParam) == EN_CHANGE && (HWND)lParam == data->hwndEdit)
        {
            data->modified = TRUE;
            Editor_UpdateLineNumbers(hwnd);
            Editor_ApplySyntaxHighlighting(hwnd);
        }
        return 0;

    case WM_DESTROY:
        if (data)
        {
            if (data->hFont)
                DeleteObject(data->hFont);
            free(data);
        }
        return 0;
    }

    return DefWindowProc(hwnd, msg, wParam, lParam);
}

/*
 * Editor_IsEditor - Check if window is an editor
 */
BOOL Editor_IsEditor(HWND hwnd)
{
    TCHAR className[256];
    GetClassName(hwnd, className, 256);
    return _tcscmp(className, EDITOR_CLASS) == 0;
}

/*
 * Editor_Clear - Clear editor content
 */
void Editor_Clear(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit)
    {
        SetWindowText(data->hwndEdit, TEXT(""));
        data->modified = FALSE;
    }
}

/*
 * Editor_SetLanguage - Set editor language for syntax highlighting
 */
void Editor_SetLanguage(HWND hwnd, EditorLanguage lang)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data)
    {
        data->language = lang;
        Editor_ApplySyntaxHighlighting(hwnd);
    }
}

/*
 * Editor_GetCurrentLine - Get current line number (1-based)
 */
int Editor_GetCurrentLine(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit)
    {
        DWORD sel = SendMessage(data->hwndEdit, EM_GETSEL, 0, 0);
        int charIndex = LOWORD(sel);
        return SendMessage(data->hwndEdit, EM_LINEFROMCHAR, charIndex, 0) + 1;
    }
    return 1;
}

/*
 * Editor_ShowFindDialog - Show find dialog
 */
void Editor_ShowFindDialog(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit)
    {
        static FINDREPLACE fr = {0};
        static TCHAR findBuf[256] = {0};

        if (!fr.lStructSize)
        {
            fr.lStructSize = sizeof(FINDREPLACE);
            fr.hwndOwner = hwnd;
            fr.lpstrFindWhat = findBuf;
            fr.wFindWhatLen = 256;
            fr.Flags = FR_DOWN;
        }

        FindText(&fr);
    }
}

/*
 * Editor_ShowReplaceDialog - Show replace dialog
 */
void Editor_ShowReplaceDialog(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit)
    {
        static FINDREPLACE fr = {0};
        static TCHAR findBuf[256] = {0};
        static TCHAR replaceBuf[256] = {0};

        if (!fr.lStructSize)
        {
            fr.lStructSize = sizeof(FINDREPLACE);
            fr.hwndOwner = hwnd;
            fr.lpstrFindWhat = findBuf;
            fr.lpstrReplaceWith = replaceBuf;
            fr.wFindWhatLen = 256;
            fr.wReplaceWithLen = 256;
            fr.Flags = FR_DOWN;
        }

        ReplaceText(&fr);
    }
}

/*
 * Editor_ShowGotoDialog - Show go to line dialog
 */
void Editor_ShowGotoDialog(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit)
    {
        TCHAR buf[32];
        if (DialogBoxParam(GetModuleHandle(NULL), TEXT("IDD_GOTO"),
                           hwnd, NULL, 0) == IDOK)
        {
            int line = _ttoi(buf);
            if (line > 0)
            {
                int charIndex = SendMessage(data->hwndEdit, EM_LINEINDEX, line - 1, 0);
                SendMessage(data->hwndEdit, EM_SETSEL, charIndex, charIndex);
                SendMessage(data->hwndEdit, EM_SCROLLCARET, 0, 0);
            }
        }
    }
}

/*
 * Editor_ToggleBookmark - Toggle bookmark at line
 */
void Editor_ToggleBookmark(HWND hwnd, int line)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && line >= 0 && line < MAX_BOOKMARKS)
    {
        data->bookmarks[line] = !data->bookmarks[line];
        if (data->bookmarks[line])
        {
            data->bookmarkCount++;
        }
        else
        {
            data->bookmarkCount--;
        }
    }
}

/*
 * Editor_GotoNextBookmark - Go to next bookmark
 */
void Editor_GotoNextBookmark(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit && data->bookmarkCount > 0)
    {
        int currentLine = Editor_GetCurrentLine(hwnd);
        for (int i = currentLine; i < MAX_BOOKMARKS; i++)
        {
            if (data->bookmarks[i])
            {
                int charIndex = SendMessage(data->hwndEdit, EM_LINEINDEX, i, 0);
                SendMessage(data->hwndEdit, EM_SETSEL, charIndex, charIndex);
                SendMessage(data->hwndEdit, EM_SCROLLCARET, 0, 0);
                return;
            }
        }
        /* Wrap around */
        for (int i = 0; i < currentLine; i++)
        {
            if (data->bookmarks[i])
            {
                int charIndex = SendMessage(data->hwndEdit, EM_LINEINDEX, i, 0);
                SendMessage(data->hwndEdit, EM_SETSEL, charIndex, charIndex);
                SendMessage(data->hwndEdit, EM_SCROLLCARET, 0, 0);
                return;
            }
        }
    }
}

/*
 * Editor_GotoPreviousBookmark - Go to previous bookmark
 */
void Editor_GotoPreviousBookmark(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit && data->bookmarkCount > 0)
    {
        int currentLine = Editor_GetCurrentLine(hwnd);
        for (int i = currentLine - 2; i >= 0; i--)
        {
            if (data->bookmarks[i])
            {
                int charIndex = SendMessage(data->hwndEdit, EM_LINEINDEX, i, 0);
                SendMessage(data->hwndEdit, EM_SETSEL, charIndex, charIndex);
                SendMessage(data->hwndEdit, EM_SCROLLCARET, 0, 0);
                return;
            }
        }
        /* Wrap around */
        for (int i = MAX_BOOKMARKS - 1; i >= currentLine; i--)
        {
            if (data->bookmarks[i])
            {
                int charIndex = SendMessage(data->hwndEdit, EM_LINEINDEX, i, 0);
                SendMessage(data->hwndEdit, EM_SETSEL, charIndex, charIndex);
                SendMessage(data->hwndEdit, EM_SCROLLCARET, 0, 0);
                return;
            }
        }
    }
}

/*
 * Editor_SetText - Set editor text
 */
void Editor_SetText(HWND hwnd, const TCHAR *text)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit)
    {
        SetWindowText(data->hwndEdit, text);
        data->modified = FALSE;
    }
}

/*
 * Editor_GetText - Get editor text
 */
int Editor_GetText(HWND hwnd, TCHAR *buffer, int maxLen)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data && data->hwndEdit)
    {
        return GetWindowText(data->hwndEdit, buffer, maxLen);
    }
    return 0;
}

/*
 * Editor_IsModified - Check if editor has been modified
 */
BOOL Editor_IsModified(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    return data ? data->modified : FALSE;
}

/*
 * Editor_SetModified - Set editor modified flag
 */
void Editor_SetModified(HWND hwnd, BOOL modified)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data)
    {
        data->modified = modified;
    }
}

/*
 * Editor_ApplySyntaxHighlighting - Apply syntax highlighting to editor
 */
static void Editor_ApplySyntaxHighlighting(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data || !data->hwndEdit)
        return;

    /* Get text length */
    int length = GetWindowTextLength(data->hwndEdit);
    if (length == 0)
        return;

    /* Allocate buffer */
    TCHAR *text = (TCHAR *)malloc((length + 1) * sizeof(TCHAR));
    if (!text)
        return;

    /* Get text */
    GetWindowText(data->hwndEdit, text, length + 1);

    /* Save current selection */
    CHARRANGE savedRange;
    SendMessage(data->hwndEdit, EM_EXGETSEL, 0, (LPARAM)&savedRange);

    /* Disable redraw */
    SendMessage(data->hwndEdit, WM_SETREDRAW, FALSE, 0);

    /* Reset all formatting */
    CHARFORMAT2 cf = {0};
    cf.cbSize = sizeof(CHARFORMAT2);
    cf.dwMask = CFM_COLOR;
    cf.crTextColor = data->fgColor;
    SendMessage(data->hwndEdit, EM_SETSEL, 0, -1);
    SendMessage(data->hwndEdit, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM)&cf);

    /* Apply keyword highlighting */
    TCHAR *p = text;
    int pos = 0;
    while (*p)
    {
        /* Check for keywords */
        if (_istalpha(*p))
        {
            TCHAR word[64];
            int wordLen = 0;
            while (_istalnum(*p) && wordLen < 63)
            {
                word[wordLen++] = *p++;
            }
            word[wordLen] = '\0';

            if (Editor_IsKeyword(word, data->language))
            {
                cf.crTextColor = g_colorKeyword;
                SendMessage(data->hwndEdit, EM_SETSEL, pos, pos + wordLen);
                SendMessage(data->hwndEdit, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM)&cf);
            }
            pos += wordLen;
        }
        /* Check for strings */
        else if (*p == '"')
        {
            int start = pos;
            p++;
            pos++;
            while (*p && *p != '"')
            {
                p++;
                pos++;
            }
            if (*p == '"')
            {
                p++;
                pos++;
            }
            cf.crTextColor = g_colorString;
            SendMessage(data->hwndEdit, EM_SETSEL, start, pos);
            SendMessage(data->hwndEdit, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM)&cf);
        }
        /* Check for comments */
        else if ((*p == '\'' && data->language == EDITOR_LANG_BASIC) ||
                 (*p == ';' && data->language == EDITOR_LANG_LOGO))
        {
            int start = pos;
            while (*p && *p != '\n')
            {
                p++;
                pos++;
            }
            cf.crTextColor = g_colorComment;
            SendMessage(data->hwndEdit, EM_SETSEL, start, pos);
            SendMessage(data->hwndEdit, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM)&cf);
        }
        /* Check for numbers */
        else if (_istdigit(*p))
        {
            int start = pos;
            while (_istdigit(*p) || *p == '.')
            {
                p++;
                pos++;
            }
            cf.crTextColor = g_colorNumber;
            SendMessage(data->hwndEdit, EM_SETSEL, start, pos);
            SendMessage(data->hwndEdit, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM)&cf);
        }
        else
        {
            p++;
            pos++;
        }
    }

    /* Restore selection */
    SendMessage(data->hwndEdit, EM_EXSETSEL, 0, (LPARAM)&savedRange);

    /* Enable redraw */
    SendMessage(data->hwndEdit, WM_SETREDRAW, TRUE, 0);
    InvalidateRect(data->hwndEdit, NULL, TRUE);

    free(text);
}

/*
 * Editor_UpdateLineNumbers - Update line numbers display
 */
static void Editor_UpdateLineNumbers(HWND hwnd)
{
    EditorData *data = (EditorData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (!data || !data->hwndEdit || !data->hwndLineNumbers)
        return;

    int lineCount = SendMessage(data->hwndEdit, EM_GETLINECOUNT, 0, 0);
    TCHAR *lineNumbers = (TCHAR *)malloc((lineCount * 10 + 1) * sizeof(TCHAR));
    if (!lineNumbers)
        return;

    lineNumbers[0] = '\0';
    for (int i = 1; i <= lineCount; i++)
    {
        TCHAR buf[16];
        wsprintf(buf, TEXT("%d\r\n"), i);
        _tcscat(lineNumbers, buf);
    }

    SetWindowText(data->hwndLineNumbers, lineNumbers);
    free(lineNumbers);
}

/*
 * Editor_IsKeyword - Check if word is a keyword
 */
static BOOL Editor_IsKeyword(const TCHAR *word, EditorLanguage lang)
{
    const TCHAR **keywords = NULL;

    switch (lang)
    {
    case EDITOR_LANG_BASIC:
        keywords = g_basicKeywords;
        break;
    case EDITOR_LANG_PILOT:
        keywords = g_pilotKeywords;
        break;
    case EDITOR_LANG_LOGO:
        keywords = g_logoKeywords;
        break;
    default:
        return FALSE;
    }

    for (int i = 0; keywords[i] != NULL; i++)
    {
        if (_tcsicmp(word, keywords[i]) == 0)
        {
            return TRUE;
        }
    }

    return FALSE;
}
