#include <windows.h>
#include <stdio.h>
#include <shlobj.h>
#include "payload.h"

// Simple installer for Time Warp IDE

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    char installDir[MAX_PATH];
    char exePath[MAX_PATH];
    FILE *fp;

    // 1. Ask user for confirmation
    if (MessageBox(NULL, "Install Time Warp IDE for Windows 2000?", "Time Warp Setup", MB_YESNO | MB_ICONQUESTION) != IDYES) {
        return 0;
    }

    // 2. Create installation directory (C:\TimeWarp)
    // Ideally we'd ask, but for Win2k simplicity let's default to C:\TimeWarp or Program Files
    // Let's try to get Program Files
    if (SHGetSpecialFolderPath(NULL, installDir, CSIDL_PROGRAM_FILES, FALSE)) {
        strcat(installDir, "\\TimeWarp");
    } else {
        strcpy(installDir, "C:\\TimeWarp");
    }

    if (!CreateDirectory(installDir, NULL)) {
        if (GetLastError() != ERROR_ALREADY_EXISTS) {
            MessageBox(NULL, "Failed to create installation directory.", "Error", MB_OK | MB_ICONERROR);
            return 1;
        }
    }

    // 3. Write the executable
    sprintf(exePath, "%s\\TimeWarpIDE.exe", installDir);
    fp = fopen(exePath, "wb");
    if (!fp) {
        char msg[512];
        sprintf(msg, "Failed to write file: %s", exePath);
        MessageBox(NULL, msg, "Error", MB_OK | MB_ICONERROR);
        return 1;
    }

    fwrite(timewarp_exe, 1, timewarp_exe_len, fp);
    fclose(fp);

    // 4. Success message
    char msg[512];
    sprintf(msg, "Installation Complete!\n\nInstalled to: %s\n\nYou can now run TimeWarpIDE.exe from that folder.", installDir);
    MessageBox(NULL, msg, "Success", MB_OK | MB_ICONINFORMATION);

    // 5. Offer to launch
    if (MessageBox(NULL, "Launch Time Warp IDE now?", "Setup", MB_YESNO | MB_ICONQUESTION) == IDYES) {
        ShellExecute(NULL, "open", exePath, NULL, installDir, SW_SHOWNORMAL);
    }

    return 0;
}
