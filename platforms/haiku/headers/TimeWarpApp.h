/*
 * Time Warp IDE - Haiku OS Edition
 * BApplication subclass - main app controller
 */

#ifndef TIMEWARP_APP_H
#define TIMEWARP_APP_H

#include <Application.h>

class TimeWarpWindow;

class TimeWarpApp : public BApplication
{
public:
    TimeWarpApp();
    virtual ~TimeWarpApp();

    virtual void ReadyToRun();
    virtual void AboutRequested();
    virtual bool QuitRequested();

private:
    TimeWarpWindow *fMainWindow;
};

#endif // TIMEWARP_APP_H
