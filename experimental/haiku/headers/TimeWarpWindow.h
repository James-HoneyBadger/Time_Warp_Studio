/*
 * Time Warp IDE - Haiku OS Edition
 * Main window with split view layout
 */

#ifndef TIMEWARP_WINDOW_H
#define TIMEWARP_WINDOW_H

#include <Window.h>

class BMenuBar;
class BMenu;
class BMenuItem;
class BSplitView;
class BButton;
class EditorView;
class OutputView;
class TurtleView;
class BStringView;

// Message constants
const uint32 MSG_FILE_NEW = 'fnew';
const uint32 MSG_FILE_OPEN = 'fopn';
const uint32 MSG_FILE_SAVE = 'fsav';
const uint32 MSG_FILE_SAVE_AS = 'fsva';
const uint32 MSG_RUN_CODE = 'runc';
const uint32 MSG_CLEAR_OUTPUT = 'clro';
const uint32 MSG_CLEAR_CANVAS = 'clrc';
const uint32 MSG_LANG_BASIC = 'lbas';
const uint32 MSG_LANG_PILOT = 'lpil';
const uint32 MSG_LANG_LOGO = 'llog';

class TimeWarpWindow : public BWindow
{
public:
    TimeWarpWindow(BRect frame);
    virtual ~TimeWarpWindow();

    virtual void MessageReceived(BMessage *message);
    virtual bool QuitRequested();

private:
    void _BuildMenu(BRect bounds);
    void _BuildUI(BRect bounds);
    void _RunCode();
    void _ClearOutput();
    void _ClearCanvas();
    void _NewFile();
    void _OpenFile();
    void _SaveFile();
    void _SaveFileAs();
    void _SetLanguage(const char *lang);

    BMenuBar *fMenuBar;
    BSplitView *fMainSplit;
    BSplitView *fRightSplit;
    EditorView *fEditor;
    OutputView *fOutput;
    TurtleView *fTurtleCanvas;
    BButton *fRunButton;
    BStringView *fStatusBar;

    BString fCurrentFile;
    BString fCurrentLanguage;
    bool fModified;
};

#endif // TIMEWARP_WINDOW_H
