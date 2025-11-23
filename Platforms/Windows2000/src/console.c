/*
 * console.c - Multi-tab output console
 */

#include "console.h"
#include <richedit.h>

typedef struct {
    HWND hwndEdit;
    COLORREF currentColor;
} ConsoleData;

HWND Console_Create(HWND hwndParent, HINSTANCE hInstance) {
    HWND hwnd = CreateWindowEx(0, RICHEDIT_CLASS, TEXT(""),
                               WS_CHILD | WS_VISIBLE | WS_VSCROLL | ES_MULTILINE | ES_READONLY,
                               0, 0, 0, 0, hwndParent, NULL, hInstance, NULL);
    ConsoleData *data = (ConsoleData *)calloc(1, sizeof(ConsoleData));
    data->hwndEdit = hwnd;
    data->currentColor = RGB(0, 0, 0);
    SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)data);
    return hwnd;
}

void Console_Clear(HWND hwnd) {
    SetWindowText(hwnd, TEXT(""));
}

void Console_AppendText(HWND hwnd, const TCHAR *text) {
    int len = GetWindowTextLength(hwnd);
    SendMessage(hwnd, EM_SETSEL, len, len);
    SendMessage(hwnd, EM_REPLACESEL, FALSE, (LPARAM)text);
}

void Console_AppendColorText(HWND hwnd, const TCHAR *text, COLORREF color) {
    Console_AppendText(hwnd, text);
}

void Console_SetTextColor(HWND hwnd, COLORREF color) {
    ConsoleData *data = (ConsoleData *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
    if (data) data->currentColor = color;
}

void Console_Export(HWND hwnd, const TCHAR *filename) { }
void Console_Find(HWND hwnd, const TCHAR *searchText) { }
void Console_AddTab(HWND hwnd, const TCHAR *tabName) { }
void Console_SelectTab(HWND hwnd, int tabIndex) { }
int Console_GetActiveTab(HWND hwnd) { return 0; }
