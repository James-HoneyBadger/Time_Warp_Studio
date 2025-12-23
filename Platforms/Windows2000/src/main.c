/*
 * Time Warp IDE for Windows 2000
 * Main Application Entry Point
 *
 * Complete MDI-based IDE with editors, canvas, interpreters, and debugger
 * Target: Windows 2000+ (WINVER 0x0500)
 */

#define WINVER 0x0500
#define _WIN32_WINNT 0x0500
#define _WIN32_IE 0x0500

#include <windows.h>
#include <commctrl.h>
#include <commdlg.h>
#include <richedit.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "editor.h"
#include "canvas.h"
#include "console.h"
#include "basic_interpreter.h"
#include "pilot_interpreter.h"
#include "logo_interpreter.h"
#include "pascal_interpreter.h"
#include "forth_interpreter.h"
#include "debugger.h"
#include "file_ops.h"

/* Global variables */
static HINSTANCE g_hInstance;
static HWND g_hwndMain;
static HWND g_hwndClient;
static HWND g_hwndStatusBar;
static HWND g_hwndToolbar;
static HWND g_hwndActiveEditor;
static HWND g_hwndActiveCanvas;
static HWND g_hwndConsole;
static HWND g_hwndDebugger;

/* Application state */
typedef enum
{
    LANG_BASIC,
    LANG_PILOT,
    LANG_LOGO,
    LANG_PASCAL,
    LANG_FORTH
} LanguageType;

typedef struct
{
    LanguageType currentLanguage;
    BOOL debugMode;
    BOOL modified;
    TCHAR currentFile[MAX_PATH];
    TCHAR projectPath[MAX_PATH];
} AppState;

static AppState g_appState = {0};

/* Menu IDs */
#define IDM_FILE_NEW 1001
#define IDM_FILE_OPEN 1002
#define IDM_FILE_SAVE 1003
#define IDM_FILE_SAVEAS 1004
#define IDM_FILE_CLOSE 1005
#define IDM_FILE_EXIT 1006
#define IDM_FILE_RECENT 1010

#define IDM_EDIT_UNDO 2001
#define IDM_EDIT_REDO 2002
#define IDM_EDIT_CUT 2003
#define IDM_EDIT_COPY 2004
#define IDM_EDIT_PASTE 2005
#define IDM_EDIT_FIND 2006
#define IDM_EDIT_REPLACE 2007
#define IDM_EDIT_GOTO 2008

#define IDM_RUN_EXECUTE 3001
#define IDM_RUN_STOP 3002
#define IDM_RUN_STEP 3003
#define IDM_RUN_CONTINUE 3004

#define IDM_DEBUG_TOGGLE 4001
#define IDM_DEBUG_BREAKPOINT 4002
#define IDM_DEBUG_WATCH 4003
#define IDM_DEBUG_CALLSTACK 4004
#define IDM_DEBUG_MEMORY 4005

#define IDM_LANG_BASIC 5001
#define IDM_LANG_PILOT 5002
#define IDM_LANG_LOGO 5003
#define IDM_LANG_PASCAL 5004
#define IDM_LANG_FORTH 5005

#define IDM_VIEW_EDITOR 6001
#define IDM_VIEW_CANVAS 6002
#define IDM_VIEW_CONSOLE 6003
#define IDM_VIEW_DEBUGGER 6004
#define IDM_VIEW_TOOLBAR 6005
#define IDM_VIEW_STATUSBAR 6006

#define IDM_WINDOW_CASCADE 7001
#define IDM_WINDOW_TILE_H 7002
#define IDM_WINDOW_TILE_V 7003
#define IDM_WINDOW_ARRANGE 7004

#define IDM_HELP_CONTENTS 8001
#define IDM_HELP_BASIC 8002
#define IDM_HELP_PILOT 8003
#define IDM_HELP_LOGO 8004
#define IDM_HELP_PASCAL 8005
#define IDM_HELP_FORTH 8006
#define IDM_HELP_ABOUT 8007

/* Toolbar button IDs */
#define IDT_NEW 9001
#define IDT_OPEN 9002
#define IDT_SAVE 9003
#define IDT_RUN 9004
#define IDT_STOP 9005
#define IDT_DEBUG 9006

/* Forward declarations */
LRESULT CALLBACK MainWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK MDIChildProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
void CreateMainMenu(HWND hwnd);
void CreateToolbar(HWND hwnd);
void CreateStatusBar(HWND hwnd);
void CreateMDIClient(HWND hwnd);
HWND CreateEditorWindow(void);
HWND CreateCanvasWindow(void);
void UpdateUI(void);
void ExecuteCode(void);
void StopExecution(void);
void ToggleDebugMode(void);
void SwitchLanguage(LanguageType lang);

/*
 * WinMain - Application entry point
 */
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow)
{
    WNDCLASSEX wc = {0};
    MSG msg;
    HACCEL hAccel;

    g_hInstance = hInstance;

    /* Initialize common controls */
    INITCOMMONCONTROLSEX icc = {0};
    icc.dwSize = sizeof(INITCOMMONCONTROLSEX);
    icc.dwICC = ICC_WIN95_CLASSES | ICC_BAR_CLASSES | ICC_COOL_CLASSES;
    InitCommonControlsEx(&icc);

    /* Load RichEdit library */
    LoadLibrary(TEXT("Riched20.dll"));

    /* Register main window class */
    wc.cbSize = sizeof(WNDCLASSEX);
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = MainWndProc;
    wc.hInstance = hInstance;
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    wc.lpszClassName = TEXT("TimeWarpMainWnd");
    wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);

    if (!RegisterClassEx(&wc))
    {
        MessageBox(NULL, TEXT("Failed to register window class"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    /* Register MDI child window class */
    wc.lpfnWndProc = MDIChildProc;
    wc.lpszClassName = TEXT("TimeWarpMDIChild");
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);

    if (!RegisterClassEx(&wc))
    {
        MessageBox(NULL, TEXT("Failed to register MDI child class"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    /* Create main window */
    g_hwndMain = CreateWindowEx(
        0,
        TEXT("TimeWarpMainWnd"),
        TEXT("Time Warp IDE - Windows 2000 Edition"),
        WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
        CW_USEDEFAULT, CW_USEDEFAULT,
        1024, 768,
        NULL, NULL, hInstance, NULL);

    if (!g_hwndMain)
    {
        MessageBox(NULL, TEXT("Failed to create main window"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    /* Initialize interpreters */
    if (!BasicInterpreter_Init())
    {
        MessageBox(g_hwndMain, TEXT("Failed to initialize BASIC interpreter"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    if (!PilotInterpreter_Init())
    {
        MessageBox(g_hwndMain, TEXT("Failed to initialize PILOT interpreter"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    if (!LogoInterpreter_Init())
    {
        MessageBox(g_hwndMain, TEXT("Failed to initialize Logo interpreter"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    if (!PascalInterpreter_Init())
    {
        MessageBox(g_hwndMain, TEXT("Failed to initialize Pascal interpreter"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    if (!ForthInterpreter_Init())
    {
        MessageBox(g_hwndMain, TEXT("Failed to initialize Forth interpreter"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    /* Initialize debugger */
    if (!Debugger_Init())
    {
        MessageBox(g_hwndMain, TEXT("Failed to initialize debugger"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return 1;
    }

    /* Initialize file operations */
    FileOps_Init();

    /* Set default language */
    g_appState.currentLanguage = LANG_BASIC;
    g_appState.debugMode = FALSE;
    g_appState.modified = FALSE;
    g_appState.currentFile[0] = '\0';
    g_appState.projectPath[0] = '\0';

    /* Show main window */
    ShowWindow(g_hwndMain, nCmdShow);
    UpdateWindow(g_hwndMain);

    /* Create initial editor and canvas */
    g_hwndActiveEditor = CreateEditorWindow();
    g_hwndActiveCanvas = CreateCanvasWindow();

    /* Load accelerators */
    hAccel = LoadAccelerators(hInstance, MAKEINTRESOURCE(100));

    /* Message loop */
    while (GetMessage(&msg, NULL, 0, 0))
    {
        if (!TranslateMDISysAccel(g_hwndClient, &msg) &&
            !TranslateAccelerator(g_hwndMain, hAccel, &msg))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }

    /* Cleanup */
    BasicInterpreter_Cleanup();
    PilotInterpreter_Cleanup();
    LogoInterpreter_Cleanup();
    PascalInterpreter_Cleanup();
    ForthInterpreter_Cleanup();
    Debugger_Cleanup();
    FileOps_Cleanup();

    return (int)msg.wParam;
}

/*
 * MainWndProc - Main window message handler
 */
LRESULT CALLBACK MainWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg)
    {
    case WM_CREATE:
        CreateMainMenu(hwnd);
        CreateToolbar(hwnd);
        CreateMDIClient(hwnd);
        CreateStatusBar(hwnd);
        g_hwndConsole = Console_Create(hwnd, g_hInstance);
        g_hwndDebugger = Debugger_CreateWindow(hwnd, g_hInstance);
        return 0;

    case WM_SIZE:
        /* Resize MDI client */
        if (g_hwndClient)
        {
            RECT rc;
            GetClientRect(hwnd, &rc);

            /* Account for toolbar */
            if (g_hwndToolbar && IsWindowVisible(g_hwndToolbar))
            {
                RECT rcToolbar;
                GetWindowRect(g_hwndToolbar, &rcToolbar);
                rc.top += rcToolbar.bottom - rcToolbar.top;
            }

            /* Account for status bar */
            if (g_hwndStatusBar && IsWindowVisible(g_hwndStatusBar))
            {
                RECT rcStatus;
                GetWindowRect(g_hwndStatusBar, &rcStatus);
                rc.bottom -= rcStatus.bottom - rcStatus.top;
            }

            MoveWindow(g_hwndClient, rc.left, rc.top,
                       rc.right - rc.left, rc.bottom - rc.top, TRUE);
        }

        /* Resize status bar */
        if (g_hwndStatusBar)
        {
            SendMessage(g_hwndStatusBar, WM_SIZE, 0, 0);
        }

        /* Resize toolbar */
        if (g_hwndToolbar)
        {
            SendMessage(g_hwndToolbar, TB_AUTOSIZE, 0, 0);
        }
        return 0;

    case WM_COMMAND:
        switch (LOWORD(wParam))
        {
        /* File menu */
        case IDM_FILE_NEW:
            if (g_appState.modified)
            {
                int result = MessageBox(hwnd,
                                        TEXT("Current file has unsaved changes. Save now?"),
                                        TEXT("Save Changes"),
                                        MB_YESNOCANCEL | MB_ICONQUESTION);
                if (result == IDCANCEL)
                    break;
                if (result == IDYES)
                {
                    SendMessage(hwnd, WM_COMMAND, IDM_FILE_SAVE, 0);
                }
            }
            Editor_Clear(g_hwndActiveEditor);
            g_appState.currentFile[0] = '\0';
            g_appState.modified = FALSE;
            UpdateUI();
            break;

        case IDM_FILE_OPEN:
            FileOps_Open(hwnd, g_hwndActiveEditor, g_appState.currentFile);
            g_appState.modified = FALSE;
            UpdateUI();
            break;

        case IDM_FILE_SAVE:
            if (g_appState.currentFile[0] == '\0')
            {
                SendMessage(hwnd, WM_COMMAND, IDM_FILE_SAVEAS, 0);
            }
            else
            {
                FileOps_Save(hwnd, g_hwndActiveEditor, g_appState.currentFile);
                g_appState.modified = FALSE;
                UpdateUI();
            }
            break;

        case IDM_FILE_SAVEAS:
            if (FileOps_SaveAs(hwnd, g_hwndActiveEditor, g_appState.currentFile))
            {
                g_appState.modified = FALSE;
                UpdateUI();
            }
            break;

        case IDM_FILE_CLOSE:
            if (g_hwndActiveEditor)
            {
                SendMessage(g_hwndActiveEditor, WM_CLOSE, 0, 0);
            }
            break;

        case IDM_FILE_EXIT:
            PostMessage(hwnd, WM_CLOSE, 0, 0);
            break;

        /* Edit menu */
        case IDM_EDIT_UNDO:
            if (g_hwndActiveEditor)
            {
                SendMessage(g_hwndActiveEditor, EM_UNDO, 0, 0);
            }
            break;

        case IDM_EDIT_REDO:
            if (g_hwndActiveEditor)
            {
                SendMessage(g_hwndActiveEditor, EM_REDO, 0, 0);
            }
            break;

        case IDM_EDIT_CUT:
            if (g_hwndActiveEditor)
            {
                SendMessage(g_hwndActiveEditor, WM_CUT, 0, 0);
            }
            break;

        case IDM_EDIT_COPY:
            if (g_hwndActiveEditor)
            {
                SendMessage(g_hwndActiveEditor, WM_COPY, 0, 0);
            }
            break;

        case IDM_EDIT_PASTE:
            if (g_hwndActiveEditor)
            {
                SendMessage(g_hwndActiveEditor, WM_PASTE, 0, 0);
            }
            break;

        case IDM_EDIT_FIND:
            if (g_hwndActiveEditor)
            {
                Editor_ShowFindDialog(g_hwndActiveEditor);
            }
            break;

        case IDM_EDIT_REPLACE:
            if (g_hwndActiveEditor)
            {
                Editor_ShowReplaceDialog(g_hwndActiveEditor);
            }
            break;

        case IDM_EDIT_GOTO:
            if (g_hwndActiveEditor)
            {
                Editor_ShowGotoDialog(g_hwndActiveEditor);
            }
            break;

        /* Run menu */
        case IDM_RUN_EXECUTE:
        case IDT_RUN:
            ExecuteCode();
            break;

        case IDM_RUN_STOP:
        case IDT_STOP:
            StopExecution();
            break;

        case IDM_RUN_STEP:
            if (g_appState.debugMode)
            {
                Debugger_Step();
            }
            break;

        case IDM_RUN_CONTINUE:
            if (g_appState.debugMode)
            {
                Debugger_Continue();
            }
            break;

        /* Debug menu */
        case IDM_DEBUG_TOGGLE:
        case IDT_DEBUG:
            ToggleDebugMode();
            break;

        case IDM_DEBUG_BREAKPOINT:
            if (g_hwndActiveEditor)
            {
                int line = Editor_GetCurrentLine(g_hwndActiveEditor);
                Debugger_ToggleBreakpoint(line);
            }
            break;

        case IDM_DEBUG_WATCH:
            Debugger_ShowWatchWindow();
            break;

        case IDM_DEBUG_CALLSTACK:
            Debugger_ShowCallStackWindow();
            break;

        case IDM_DEBUG_MEMORY:
            Debugger_ShowMemoryWindow();
            break;

        /* Language menu */
        case IDM_LANG_BASIC:
            SwitchLanguage(LANG_BASIC);
            break;

        case IDM_LANG_PILOT:
            SwitchLanguage(LANG_PILOT);
            break;

        case IDM_LANG_LOGO:
            SwitchLanguage(LANG_LOGO);
            break;

        case IDM_LANG_PASCAL:
            SwitchLanguage(LANG_PASCAL);
            break;

        case IDM_LANG_FORTH:
            SwitchLanguage(LANG_FORTH);
            break;

        /* View menu */
        case IDM_VIEW_EDITOR:
            if (!g_hwndActiveEditor || !IsWindowVisible(g_hwndActiveEditor))
            {
                g_hwndActiveEditor = CreateEditorWindow();
            }
            ShowWindow(g_hwndActiveEditor, SW_SHOW);
            SetFocus(g_hwndActiveEditor);
            break;

        case IDM_VIEW_CANVAS:
            if (!g_hwndActiveCanvas || !IsWindowVisible(g_hwndActiveCanvas))
            {
                g_hwndActiveCanvas = CreateCanvasWindow();
            }
            ShowWindow(g_hwndActiveCanvas, SW_SHOW);
            break;

        case IDM_VIEW_CONSOLE:
            ShowWindow(g_hwndConsole, IsWindowVisible(g_hwndConsole) ? SW_HIDE : SW_SHOW);
            break;

        case IDM_VIEW_DEBUGGER:
            ShowWindow(g_hwndDebugger, IsWindowVisible(g_hwndDebugger) ? SW_HIDE : SW_SHOW);
            break;

        case IDM_VIEW_TOOLBAR:
            ShowWindow(g_hwndToolbar, IsWindowVisible(g_hwndToolbar) ? SW_HIDE : SW_SHOW);
            SendMessage(hwnd, WM_SIZE, 0, 0);
            break;

        case IDM_VIEW_STATUSBAR:
            ShowWindow(g_hwndStatusBar, IsWindowVisible(g_hwndStatusBar) ? SW_HIDE : SW_SHOW);
            SendMessage(hwnd, WM_SIZE, 0, 0);
            break;

        /* Window menu */
        case IDM_WINDOW_CASCADE:
            SendMessage(g_hwndClient, WM_MDICASCADE, 0, 0);
            break;

        case IDM_WINDOW_TILE_H:
            SendMessage(g_hwndClient, WM_MDITILE, MDITILE_HORIZONTAL, 0);
            break;

        case IDM_WINDOW_TILE_V:
            SendMessage(g_hwndClient, WM_MDITILE, MDITILE_VERTICAL, 0);
            break;

        case IDM_WINDOW_ARRANGE:
            SendMessage(g_hwndClient, WM_MDIICONARRANGE, 0, 0);
            break;

        /* Help menu */
        case IDM_HELP_ABOUT:
            MessageBox(hwnd,
                       TEXT("Time Warp IDE for Windows 2000\n\n")
                           TEXT("Educational Programming Environment\n")
                               TEXT("Supporting BASIC, PILOT, Logo, Pascal, and Forth\n\n")
                                   TEXT("Version 5.1.0\n")
                                       TEXT("© 2025 James Temple"),
                       TEXT("About Time Warp IDE"),
                       MB_OK | MB_ICONINFORMATION);
            break;

        /* Toolbar buttons */
        case IDT_NEW:
            SendMessage(hwnd, WM_COMMAND, IDM_FILE_NEW, 0);
            break;

        case IDT_OPEN:
            SendMessage(hwnd, WM_COMMAND, IDM_FILE_OPEN, 0);
            break;

        case IDT_SAVE:
            SendMessage(hwnd, WM_COMMAND, IDM_FILE_SAVE, 0);
            break;

        default:
            return DefFrameProc(hwnd, g_hwndClient, msg, wParam, lParam);
        }
        return 0;

    case WM_CLOSE:
        if (g_appState.modified)
        {
            int result = MessageBox(hwnd,
                                    TEXT("Current file has unsaved changes. Save now?"),
                                    TEXT("Save Changes"),
                                    MB_YESNOCANCEL | MB_ICONQUESTION);
            if (result == IDCANCEL)
                return 0;
            if (result == IDYES)
            {
                SendMessage(hwnd, WM_COMMAND, IDM_FILE_SAVE, 0);
            }
        }
        DestroyWindow(hwnd);
        return 0;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;

    default:
        return DefFrameProc(hwnd, g_hwndClient, msg, wParam, lParam);
    }
}

/*
 * MDIChildProc - MDI child window message handler
 */
LRESULT CALLBACK MDIChildProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg)
    {
    case WM_CREATE:
        return 0;

    case WM_MDIACTIVATE:
        /* Update active window references */
        if ((HWND)lParam == hwnd)
        {
            /* This window is being activated */
            HWND hwndChild = GetWindow(hwnd, GW_CHILD);
            if (hwndChild)
            {
                if (Editor_IsEditor(hwndChild))
                {
                    g_hwndActiveEditor = hwndChild;
                }
                else if (Canvas_IsCanvas(hwndChild))
                {
                    g_hwndActiveCanvas = hwndChild;
                }
            }
        }
        UpdateUI();
        return 0;

    case WM_CLOSE:
        DestroyWindow(hwnd);
        return 0;

    default:
        return DefMDIChildProc(hwnd, msg, wParam, lParam);
    }
}

/*
 * CreateMainMenu - Create application menu bar
 */
void CreateMainMenu(HWND hwnd)
{
    HMENU hMenu = CreateMenu();
    HMENU hSubMenu;

    /* File menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_FILE_NEW, TEXT("&New\tCtrl+N"));
    AppendMenu(hSubMenu, MF_STRING, IDM_FILE_OPEN, TEXT("&Open...\tCtrl+O"));
    AppendMenu(hSubMenu, MF_STRING, IDM_FILE_SAVE, TEXT("&Save\tCtrl+S"));
    AppendMenu(hSubMenu, MF_STRING, IDM_FILE_SAVEAS, TEXT("Save &As..."));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_FILE_CLOSE, TEXT("&Close\tCtrl+W"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_FILE_EXIT, TEXT("E&xit\tAlt+F4"));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&File"));

    /* Edit menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_UNDO, TEXT("&Undo\tCtrl+Z"));
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_REDO, TEXT("&Redo\tCtrl+Y"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_CUT, TEXT("Cu&t\tCtrl+X"));
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_COPY, TEXT("&Copy\tCtrl+C"));
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_PASTE, TEXT("&Paste\tCtrl+V"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_FIND, TEXT("&Find...\tCtrl+F"));
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_REPLACE, TEXT("&Replace...\tCtrl+H"));
    AppendMenu(hSubMenu, MF_STRING, IDM_EDIT_GOTO, TEXT("&Go To Line...\tCtrl+G"));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&Edit"));

    /* Run menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_RUN_EXECUTE, TEXT("&Execute\tF5"));
    AppendMenu(hSubMenu, MF_STRING, IDM_RUN_STOP, TEXT("&Stop\tShift+F5"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_RUN_STEP, TEXT("Step &Into\tF11"));
    AppendMenu(hSubMenu, MF_STRING, IDM_RUN_CONTINUE, TEXT("&Continue\tF5"));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&Run"));

    /* Debug menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_DEBUG_TOGGLE, TEXT("Toggle Debug &Mode\tF9"));
    AppendMenu(hSubMenu, MF_STRING, IDM_DEBUG_BREAKPOINT, TEXT("Toggle &Breakpoint\tF8"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_DEBUG_WATCH, TEXT("&Watch..."));
    AppendMenu(hSubMenu, MF_STRING, IDM_DEBUG_CALLSTACK, TEXT("Call &Stack..."));
    AppendMenu(hSubMenu, MF_STRING, IDM_DEBUG_MEMORY, TEXT("&Memory View..."));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&Debug"));

    /* Language menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_LANG_BASIC, TEXT("&BASIC"));
    AppendMenu(hSubMenu, MF_STRING, IDM_LANG_PILOT, TEXT("&PILOT"));
    AppendMenu(hSubMenu, MF_STRING, IDM_LANG_LOGO, TEXT("&Logo"));
    AppendMenu(hSubMenu, MF_STRING, IDM_LANG_PASCAL, TEXT("&Pascal"));
    AppendMenu(hSubMenu, MF_STRING, IDM_LANG_FORTH, TEXT("&Forth"));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&Language"));

    /* View menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_VIEW_EDITOR, TEXT("&Editor"));
    AppendMenu(hSubMenu, MF_STRING, IDM_VIEW_CANVAS, TEXT("&Canvas"));
    AppendMenu(hSubMenu, MF_STRING, IDM_VIEW_CONSOLE, TEXT("C&onsole"));
    AppendMenu(hSubMenu, MF_STRING, IDM_VIEW_DEBUGGER, TEXT("&Debugger"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_VIEW_TOOLBAR, TEXT("&Toolbar"));
    AppendMenu(hSubMenu, MF_STRING, IDM_VIEW_STATUSBAR, TEXT("&Status Bar"));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&View"));

    /* Window menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_WINDOW_CASCADE, TEXT("&Cascade"));
    AppendMenu(hSubMenu, MF_STRING, IDM_WINDOW_TILE_H, TEXT("Tile &Horizontally"));
    AppendMenu(hSubMenu, MF_STRING, IDM_WINDOW_TILE_V, TEXT("Tile &Vertically"));
    AppendMenu(hSubMenu, MF_STRING, IDM_WINDOW_ARRANGE, TEXT("&Arrange Icons"));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&Window"));

    /* Help menu */
    hSubMenu = CreatePopupMenu();
    AppendMenu(hSubMenu, MF_STRING, IDM_HELP_CONTENTS, TEXT("&Contents\tF1"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_HELP_BASIC, TEXT("&BASIC Reference"));
    AppendMenu(hSubMenu, MF_STRING, IDM_HELP_PILOT, TEXT("&PILOT Reference"));
    AppendMenu(hSubMenu, MF_STRING, IDM_HELP_LOGO, TEXT("&Logo Reference"));
    AppendMenu(hSubMenu, MF_STRING, IDM_HELP_PASCAL, TEXT("&Pascal Reference"));
    AppendMenu(hSubMenu, MF_STRING, IDM_HELP_FORTH, TEXT("&Forth Reference"));
    AppendMenu(hSubMenu, MF_SEPARATOR, 0, NULL);
    AppendMenu(hSubMenu, MF_STRING, IDM_HELP_ABOUT, TEXT("&About Time Warp IDE"));
    AppendMenu(hMenu, MF_POPUP, (UINT_PTR)hSubMenu, TEXT("&Help"));

    SetMenu(hwnd, hMenu);
}

/*
 * CreateToolbar - Create application toolbar
 */
void CreateToolbar(HWND hwnd)
{
    TBBUTTON buttons[] = {
        {0, IDT_NEW, TBSTATE_ENABLED, TBSTYLE_BUTTON, {0}, 0, (INT_PTR)TEXT("New")},
        {1, IDT_OPEN, TBSTATE_ENABLED, TBSTYLE_BUTTON, {0}, 0, (INT_PTR)TEXT("Open")},
        {2, IDT_SAVE, TBSTATE_ENABLED, TBSTYLE_BUTTON, {0}, 0, (INT_PTR)TEXT("Save")},
        {0, 0, 0, TBSTYLE_SEP, {0}, 0, 0},
        {3, IDT_RUN, TBSTATE_ENABLED, TBSTYLE_BUTTON, {0}, 0, (INT_PTR)TEXT("Run")},
        {4, IDT_STOP, TBSTATE_ENABLED, TBSTYLE_BUTTON, {0}, 0, (INT_PTR)TEXT("Stop")},
        {0, 0, 0, TBSTYLE_SEP, {0}, 0, 0},
        {5, IDT_DEBUG, TBSTATE_ENABLED, TBSTYLE_BUTTON, {0}, 0, (INT_PTR)TEXT("Debug")}};

    g_hwndToolbar = CreateWindowEx(
        0,
        TOOLBARCLASSNAME,
        NULL,
        WS_CHILD | WS_VISIBLE | TBSTYLE_FLAT | TBSTYLE_TOOLTIPS,
        0, 0, 0, 0,
        hwnd,
        NULL,
        g_hInstance,
        NULL);

    SendMessage(g_hwndToolbar, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0);
    SendMessage(g_hwndToolbar, TB_ADDBUTTONS, sizeof(buttons) / sizeof(TBBUTTON),
                (LPARAM)&buttons);
    SendMessage(g_hwndToolbar, TB_AUTOSIZE, 0, 0);
}

/*
 * CreateStatusBar - Create application status bar
 */
void CreateStatusBar(HWND hwnd)
{
    int parts[] = {200, 400, -1};

    g_hwndStatusBar = CreateWindowEx(
        0,
        STATUSCLASSNAME,
        NULL,
        WS_CHILD | WS_VISIBLE | SBARS_SIZEGRIP,
        0, 0, 0, 0,
        hwnd,
        NULL,
        g_hInstance,
        NULL);

    SendMessage(g_hwndStatusBar, SB_SETPARTS, 3, (LPARAM)parts);
    SendMessage(g_hwndStatusBar, SB_SETTEXT, 0, (LPARAM)TEXT("Ready"));
    SendMessage(g_hwndStatusBar, SB_SETTEXT, 1, (LPARAM)TEXT("BASIC"));
    SendMessage(g_hwndStatusBar, SB_SETTEXT, 2, (LPARAM)TEXT("Line 1, Col 1"));
}

/*
 * CreateMDIClient - Create MDI client window
 */
void CreateMDIClient(HWND hwnd)
{
    CLIENTCREATESTRUCT ccs = {0};
    ccs.hWindowMenu = NULL;
    ccs.idFirstChild = 50000;

    g_hwndClient = CreateWindowEx(
        WS_EX_CLIENTEDGE,
        TEXT("MDICLIENT"),
        NULL,
        WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_VSCROLL | WS_HSCROLL,
        0, 0, 0, 0,
        hwnd,
        NULL,
        g_hInstance,
        &ccs);
}

/*
 * CreateEditorWindow - Create new editor MDI child
 */
HWND CreateEditorWindow(void)
{
    MDICREATESTRUCT mcs = {0};
    mcs.szClass = TEXT("TimeWarpMDIChild");
    mcs.szTitle = TEXT("Editor");
    mcs.hOwner = g_hInstance;
    mcs.x = CW_USEDEFAULT;
    mcs.y = CW_USEDEFAULT;
    mcs.cx = CW_USEDEFAULT;
    mcs.cy = CW_USEDEFAULT;
    mcs.style = 0;
    mcs.lParam = 0;

    HWND hwndFrame = (HWND)SendMessage(g_hwndClient, WM_MDICREATE, 0, (LPARAM)&mcs);
    if (hwndFrame)
    {
        HWND hwndEditor = Editor_Create(hwndFrame, g_hInstance);
        if (hwndEditor)
        {
            RECT rc;
            GetClientRect(hwndFrame, &rc);
            MoveWindow(hwndEditor, 0, 0, rc.right, rc.bottom, TRUE);
        }
        return hwndEditor;
    }
    return NULL;
}

/*
 * CreateCanvasWindow - Create new canvas MDI child
 */
HWND CreateCanvasWindow(void)
{
    MDICREATESTRUCT mcs = {0};
    mcs.szClass = TEXT("TimeWarpMDIChild");
    mcs.szTitle = TEXT("Graphics Canvas");
    mcs.hOwner = g_hInstance;
    mcs.x = CW_USEDEFAULT;
    mcs.y = CW_USEDEFAULT;
    mcs.cx = CW_USEDEFAULT;
    mcs.cy = CW_USEDEFAULT;
    mcs.style = 0;
    mcs.lParam = 0;

    HWND hwndFrame = (HWND)SendMessage(g_hwndClient, WM_MDICREATE, 0, (LPARAM)&mcs);
    if (hwndFrame)
    {
        HWND hwndCanvas = Canvas_Create(hwndFrame, g_hInstance);
        if (hwndCanvas)
        {
            RECT rc;
            GetClientRect(hwndFrame, &rc);
            MoveWindow(hwndCanvas, 0, 0, rc.right, rc.bottom, TRUE);
        }
        return hwndCanvas;
    }
    return NULL;
}

/*
 * UpdateUI - Update UI elements based on current state
 */
void UpdateUI(void)
{
    TCHAR status[512];
    const TCHAR *langName;

    /* Update language in status bar */
    switch (g_appState.currentLanguage)
    {
    case LANG_BASIC:
        langName = TEXT("BASIC");
        break;
    case LANG_PILOT:
        langName = TEXT("PILOT");
        break;
    case LANG_LOGO:
        langName = TEXT("Logo");
        break;
    case LANG_PASCAL:
        langName = TEXT("Pascal");
        break;
    case LANG_FORTH:
        langName = TEXT("Forth");
        break;
    default:
        langName = TEXT("Unknown");
        break;
    }

    wsprintf(status, TEXT("%s%s"), langName,
             g_appState.debugMode ? TEXT(" [Debug]") : TEXT(""));
    SendMessage(g_hwndStatusBar, SB_SETTEXT, 1, (LPARAM)status);

    /* Update title bar */
    wsprintf(status, TEXT("Time Warp IDE - %s%s"),
             g_appState.currentFile[0] ? g_appState.currentFile : TEXT("Untitled"),
             g_appState.modified ? TEXT(" *") : TEXT(""));
    SetWindowText(g_hwndMain, status);
}

/*
 * ExecuteCode - Execute code from active editor
 */
void ExecuteCode(void)
{
    if (!g_hwndActiveEditor)
    {
        MessageBox(g_hwndMain, TEXT("No active editor"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return;
    }

    /* Get code from editor */
    int length = GetWindowTextLength(g_hwndActiveEditor);
    TCHAR *code = (TCHAR *)malloc((length + 1) * sizeof(TCHAR));
    if (!code)
    {
        MessageBox(g_hwndMain, TEXT("Out of memory"),
                   TEXT("Error"), MB_ICONERROR | MB_OK);
        return;
    }

    GetWindowText(g_hwndActiveEditor, code, length + 1);

    /* Clear canvas and console */
    Canvas_Clear(g_hwndActiveCanvas);
    Console_Clear(g_hwndConsole);

    /* Execute code based on current language */
    BOOL success = FALSE;
    switch (g_appState.currentLanguage)
    {
    case LANG_BASIC:
        success = BasicInterpreter_Execute(code, g_hwndConsole,
                                           g_hwndActiveCanvas,
                                           g_appState.debugMode);
        break;

    case LANG_PILOT:
        success = PilotInterpreter_Execute(code, g_hwndConsole,
                                           g_hwndActiveCanvas,
                                           g_appState.debugMode);
        break;

    case LANG_LOGO:
        success = LogoInterpreter_Execute(code, g_hwndConsole,
                                          g_hwndActiveCanvas,
                                          g_appState.debugMode);
        break;

    case LANG_PASCAL:
        success = PascalInterpreter_Execute(code, g_hwndConsole,
                                            g_hwndActiveCanvas,
                                            g_appState.debugMode);
        break;

    case LANG_FORTH:
        success = ForthInterpreter_Execute(code, g_hwndConsole,
                                           g_hwndActiveCanvas,
                                           g_appState.debugMode);
        break;
    }

    free(code);

    if (!success)
    {
        Console_AppendText(g_hwndConsole, TEXT("❌ Execution failed\n"));
    }

    SendMessage(g_hwndStatusBar, SB_SETTEXT, 0,
                (LPARAM)(success ? TEXT("✅ Execution completed") : TEXT("❌ Execution failed")));
}

/*
 * StopExecution - Stop running code
 */
void StopExecution(void)
{
    BasicInterpreter_Stop();
    PilotInterpreter_Stop();
    LogoInterpreter_Stop();
    PascalInterpreter_Stop();
    ForthInterpreter_Stop();

    SendMessage(g_hwndStatusBar, SB_SETTEXT, 0, (LPARAM)TEXT("🛑 Execution stopped"));
}

/*
 * ToggleDebugMode - Toggle debug mode on/off
 */
void ToggleDebugMode(void)
{
    g_appState.debugMode = !g_appState.debugMode;
    UpdateUI();

    if (g_appState.debugMode)
    {
        ShowWindow(g_hwndDebugger, SW_SHOW);
    }
}

/*
 * SwitchLanguage - Switch between BASIC/PILOT/Logo
 */
void SwitchLanguage(LanguageType lang)
{
    g_appState.currentLanguage = lang;
    UpdateUI();

    /* Update editor syntax highlighting */
    if (g_hwndActiveEditor)
    {
        Editor_SetLanguage(g_hwndActiveEditor, lang);
    }
}
