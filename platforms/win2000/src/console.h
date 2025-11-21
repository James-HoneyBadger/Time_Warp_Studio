/*
 * console.h - Multi-tab output console with color support
 * Supports ANSI colors, clear, export, find functionality
 */

#ifndef CONSOLE_H
#define CONSOLE_H

#include <windows.h>

/* Console functions */
HWND Console_Create(HWND hwndParent, HINSTANCE hInstance);
void Console_Clear(HWND hwnd);
void Console_AppendText(HWND hwnd, const TCHAR *text);
void Console_AppendColorText(HWND hwnd, const TCHAR *text, COLORREF color);
void Console_SetTextColor(HWND hwnd, COLORREF color);
void Console_Export(HWND hwnd, const TCHAR *filename);
void Console_Find(HWND hwnd, const TCHAR *searchText);
void Console_AddTab(HWND hwnd, const TCHAR *tabName);
void Console_SelectTab(HWND hwnd, int tabIndex);
int Console_GetActiveTab(HWND hwnd);

#endif /* CONSOLE_H */
