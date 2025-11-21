/*
 * editor.h - Syntax-highlighting code editor for Time Warp IDE
 * Supports BASIC, PILOT, and Logo with line numbers, bookmarks, find/replace
 */

#ifndef EDITOR_H
#define EDITOR_H

#include <windows.h>

/* Editor types */
typedef enum
{
    EDITOR_LANG_BASIC,
    EDITOR_LANG_PILOT,
    EDITOR_LANG_LOGO
} EditorLanguage;

/* Editor functions */
HWND Editor_Create(HWND hwndParent, HINSTANCE hInstance);
BOOL Editor_IsEditor(HWND hwnd);
void Editor_Clear(HWND hwnd);
void Editor_SetLanguage(HWND hwnd, EditorLanguage lang);
int Editor_GetCurrentLine(HWND hwnd);
void Editor_ShowFindDialog(HWND hwnd);
void Editor_ShowReplaceDialog(HWND hwnd);
void Editor_ShowGotoDialog(HWND hwnd);
void Editor_ToggleBookmark(HWND hwnd, int line);
void Editor_GotoNextBookmark(HWND hwnd);
void Editor_GotoPreviousBookmark(HWND hwnd);
void Editor_SetText(HWND hwnd, const TCHAR *text);
int Editor_GetText(HWND hwnd, TCHAR *buffer, int maxLen);
BOOL Editor_IsModified(HWND hwnd);
void Editor_SetModified(HWND hwnd, BOOL modified);

#endif /* EDITOR_H */
