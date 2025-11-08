/*
 * Time Warp IDE - Haiku OS Edition
 * BApplication implementation
 */

#include "TimeWarpApp.h"
#include "TimeWarpWindow.h"
#include <Alert.h>
#include <String.h>

const char *kAppSignature = "application/x-vnd.HoneyBadger-TimeWarp";

TimeWarpApp::TimeWarpApp()
    : BApplication(kAppSignature),
      fMainWindow(NULL)
{
}

TimeWarpApp::~TimeWarpApp()
{
    // Window deletes itself
}

void TimeWarpApp::ReadyToRun()
{
    // Create main window centered on screen
    BRect frame(100, 100, 900, 700);
    fMainWindow = new TimeWarpWindow(frame);
    fMainWindow->Show();
}

void TimeWarpApp::AboutRequested()
{
    BString aboutText;
    aboutText << "Time Warp IDE - Haiku Edition\n\n"
              << "Educational programming environment\n"
              << "Languages: BASIC, PILOT, Logo\n\n"
              << "Built for Haiku OS\n"
              << "© 2025 James Temple";

    BAlert *alert = new BAlert("About Time Warp",
                               aboutText.String(),
                               "OK",
                               NULL, NULL,
                               B_WIDTH_AS_USUAL,
                               B_INFO_ALERT);
    alert->Go();
}

bool TimeWarpApp::QuitRequested()
{
    return true;
}
