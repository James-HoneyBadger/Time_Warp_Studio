#define INCL_WIN
#include <os2.h>
#include <string.h>
#include "pm_app.h"

HAB hab;
HMQ hmq;
HWND hwndFrame;
HWND hwndClient;

int main(void)
{
    QMSG qmsg;
    hab = WinInitialize(0);
    if (!hab)
        return 1;
    hmq = WinCreateMsgQueue(hab, 0);
    if (!hmq)
    {
        WinTerminate(hab);
        return 1;
    }

    if (!WinRegisterClass(hab, "TWCLIENT", client_wnd_proc, CS_SIZEREDRAW, 0))
    {
        WinDestroyMsgQueue(hmq);
        WinTerminate(hab);
        return 1;
    }

    hwndFrame = create_frame_window();
    if (!hwndFrame)
    {
        WinDestroyMsgQueue(hmq);
        WinTerminate(hab);
        return 1;
    }

    WinShowWindow(hwndFrame, TRUE);
    WinSetFocus(HWND_DESKTOP, hwndClient);

    while (WinGetMsg(hab, &qmsg, 0, 0, 0))
    {
        WinDispatchMsg(hab, &qmsg);
    }

    WinDestroyWindow(hwndFrame);
    WinDestroyMsgQueue(hmq);
    WinTerminate(hab);
    return 0;
}