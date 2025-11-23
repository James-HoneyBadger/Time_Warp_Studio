/*
 * file_ops.c - File operations
 */

#include "file_ops.h"
#include <commdlg.h>
#include <stdio.h>
#include <tchar.h>

#ifndef _MSC_VER
#define _tfopen fopen
#define _tcscpy strcpy
#endif

static TCHAR g_recentFiles[10][MAX_PATH];
static int g_recentCount = 0;

void FileOps_Init(void) {
    g_recentCount = 0;
}

void FileOps_Cleanup(void) { }

BOOL FileOps_New(HWND hwndParent, HWND hwndEditor) {
    return TRUE;
}

BOOL FileOps_Open(HWND hwndParent, HWND hwndEditor, TCHAR *filename) {
    OPENFILENAME ofn = {0};
    TCHAR szFile[MAX_PATH] = {0};
    
    ofn.lStructSize = sizeof(OPENFILENAME);
    ofn.hwndOwner = hwndParent;
    ofn.lpstrFile = szFile;
    ofn.nMaxFile = MAX_PATH;
    ofn.lpstrFilter = TEXT("Time Warp Files\0*.twb;*.twp;*.twl\0All Files\0*.*\0");
    ofn.nFilterIndex = 1;
    ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;
    
    if (GetOpenFileName(&ofn)) {
        FILE *fp = _tfopen(szFile, TEXT("r"));
        if (fp) {
            fseek(fp, 0, SEEK_END);
            long size = ftell(fp);
            fseek(fp, 0, SEEK_SET);
            TCHAR *buffer = (TCHAR *)malloc(size + 1);
            fread(buffer, 1, size, fp);
            buffer[size] = 0;
            fclose(fp);
            SetWindowText(hwndEditor, buffer);
            free(buffer);
            _tcscpy(filename, szFile);
            FileOps_AddRecentFile(szFile);
            return TRUE;
        }
    }
    return FALSE;
}

BOOL FileOps_Save(HWND hwndParent, HWND hwndEditor, const TCHAR *filename) {
    int len = GetWindowTextLength(hwndEditor);
    TCHAR *buffer = (TCHAR *)malloc((len + 1) * sizeof(TCHAR));
    GetWindowText(hwndEditor, buffer, len + 1);
    FILE *fp = _tfopen(filename, TEXT("w"));
    if (fp) {
        fwrite(buffer, sizeof(TCHAR), len, fp);
        fclose(fp);
        free(buffer);
        return TRUE;
    }
    free(buffer);
    return FALSE;
}

BOOL FileOps_SaveAs(HWND hwndParent, HWND hwndEditor, TCHAR *filename) {
    OPENFILENAME ofn = {0};
    TCHAR szFile[MAX_PATH] = {0};
    
    ofn.lStructSize = sizeof(OPENFILENAME);
    ofn.hwndOwner = hwndParent;
    ofn.lpstrFile = szFile;
    ofn.nMaxFile = MAX_PATH;
    ofn.lpstrFilter = TEXT("Time Warp Files\0*.twb;*.twp;*.twl\0All Files\0*.*\0");
    ofn.nFilterIndex = 1;
    ofn.Flags = OFN_OVERWRITEPROMPT;
    
    if (GetSaveFileName(&ofn)) {
        _tcscpy(filename, szFile);
        return FileOps_Save(hwndParent, hwndEditor, filename);
    }
    return FALSE;
}

void FileOps_AddRecentFile(const TCHAR *filename) {
    if (g_recentCount < 10) {
        _tcscpy(g_recentFiles[g_recentCount++], filename);
    }
}

int FileOps_GetRecentFileCount(void) { return g_recentCount; }
const TCHAR *FileOps_GetRecentFile(int index) { return g_recentFiles[index]; }
BOOL FileOps_LoadProject(HWND hwndParent, const TCHAR *projectFile) { return TRUE; }
BOOL FileOps_SaveProject(HWND hwndParent, const TCHAR *projectFile) { return TRUE; }
