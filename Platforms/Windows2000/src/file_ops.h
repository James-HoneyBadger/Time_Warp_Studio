/*
 * file_ops.h - File operations for Time Warp IDE
 * Supports New, Open, Save, Save As, Recent files, project management
 */

#ifndef FILE_OPS_H
#define FILE_OPS_H

#include <windows.h>

/* File operation functions */
void FileOps_Init(void);
void FileOps_Cleanup(void);
BOOL FileOps_New(HWND hwndParent, HWND hwndEditor);
BOOL FileOps_Open(HWND hwndParent, HWND hwndEditor, TCHAR *filename);
BOOL FileOps_Save(HWND hwndParent, HWND hwndEditor, const TCHAR *filename);
BOOL FileOps_SaveAs(HWND hwndParent, HWND hwndEditor, TCHAR *filename);
void FileOps_AddRecentFile(const TCHAR *filename);
int FileOps_GetRecentFileCount(void);
const TCHAR *FileOps_GetRecentFile(int index);
BOOL FileOps_LoadProject(HWND hwndParent, const TCHAR *projectFile);
BOOL FileOps_SaveProject(HWND hwndParent, const TCHAR *projectFile);

#endif /* FILE_OPS_H */
