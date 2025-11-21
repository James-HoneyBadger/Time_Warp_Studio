/*
 * Time Warp IDE - Haiku OS Edition
 * Main window implementation
 */

#include "TimeWarpWindow.h"
#include "EditorView.h"
#include "OutputView.h"
#include "TurtleView.h"
#include <Application.h>
#include <LayoutBuilder.h>
#include <MenuBar.h>
#include <Menu.h>
#include <MenuItem.h>
#include <SplitView.h>
#include <Button.h>
#include <StringView.h>
#include <FilePanel.h>
#include <Alert.h>

TimeWarpWindow::TimeWarpWindow(BRect frame)
    : BWindow(frame, "Time Warp IDE", B_TITLED_WINDOW,
              B_ASYNCHRONOUS_CONTROLS | B_AUTO_UPDATE_SIZE_LIMITS),
      fMenuBar(NULL),
      fMainSplit(NULL),
      fRightSplit(NULL),
      fEditor(NULL),
      fOutput(NULL),
      fTurtleCanvas(NULL),
      fRunButton(NULL),
      fStatusBar(NULL),
      fCurrentLanguage("BASIC"),
      fModified(false)
{
    BRect bounds = Bounds();
    _BuildMenu(bounds);
    _BuildUI(bounds);
}

TimeWarpWindow::~TimeWarpWindow()
{
}

void TimeWarpWindow::_BuildMenu(BRect bounds)
{
    fMenuBar = new BMenuBar("MenuBar");

    // File menu
    BMenu *fileMenu = new BMenu("File");
    fileMenu->AddItem(new BMenuItem("New", new BMessage(MSG_FILE_NEW), 'N'));
    fileMenu->AddItem(new BMenuItem("Open" B_UTF8_ELLIPSIS, new BMessage(MSG_FILE_OPEN), 'O'));
    fileMenu->AddSeparatorItem();
    fileMenu->AddItem(new BMenuItem("Save", new BMessage(MSG_FILE_SAVE), 'S'));
    fileMenu->AddItem(new BMenuItem("Save As" B_UTF8_ELLIPSIS, new BMessage(MSG_FILE_SAVE_AS)));
    fileMenu->AddSeparatorItem();
    fileMenu->AddItem(new BMenuItem("Quit", new BMessage(B_QUIT_REQUESTED), 'Q'));
    fMenuBar->AddItem(fileMenu);

    // Run menu
    BMenu *runMenu = new BMenu("Run");
    runMenu->AddItem(new BMenuItem("Run Code", new BMessage(MSG_RUN_CODE), 'R'));
    runMenu->AddSeparatorItem();
    runMenu->AddItem(new BMenuItem("Clear Output", new BMessage(MSG_CLEAR_OUTPUT)));
    runMenu->AddItem(new BMenuItem("Clear Canvas", new BMessage(MSG_CLEAR_CANVAS)));
    fMenuBar->AddItem(runMenu);

    // Language menu
    BMenu *langMenu = new BMenu("Language");
    BMenuItem *basicItem = new BMenuItem("BASIC", new BMessage(MSG_LANG_BASIC));
    BMenuItem *pilotItem = new BMenuItem("PILOT", new BMessage(MSG_LANG_PILOT));
    BMenuItem *logoItem = new BMenuItem("Logo", new BMessage(MSG_LANG_LOGO));
    basicItem->SetMarked(true); // Default
    langMenu->AddItem(basicItem);
    langMenu->AddItem(pilotItem);
    langMenu->AddItem(logoItem);
    langMenu->SetRadioMode(true);
    fMenuBar->AddItem(langMenu);

    // Help menu
    BMenu *helpMenu = new BMenu("Help");
    helpMenu->AddItem(new BMenuItem("About Time Warp", new BMessage(B_ABOUT_REQUESTED)));
    fMenuBar->AddItem(helpMenu);

    AddChild(fMenuBar);
}

void TimeWarpWindow::_BuildUI(BRect bounds)
{
    // Create views
    fEditor = new EditorView("Editor");
    fOutput = new OutputView("Output");
    fTurtleCanvas = new TurtleView("TurtleCanvas");

    fRunButton = new BButton("Run", new BMessage(MSG_RUN_CODE));
    fStatusBar = new BStringView("StatusBar", "Ready");

    // Layout using BSplitView
    fRightSplit = new BSplitView(B_VERTICAL);
    fRightSplit->AddChild(fOutput);
    fRightSplit->AddChild(fTurtleCanvas);

    fMainSplit = new BSplitView(B_HORIZONTAL);
    fMainSplit->AddChild(fEditor);
    fMainSplit->AddChild(fRightSplit);

    // Build complete layout
    BLayoutBuilder::Group<>(this, B_VERTICAL, 0)
        .Add(fMenuBar)
        .Add(fMainSplit)
        .AddGroup(B_HORIZONTAL)
        .Add(fRunButton)
        .AddGlue()
        .Add(fStatusBar)
        .End()
        .End();
}

void TimeWarpWindow::MessageReceived(BMessage *message)
{
    switch (message->what)
    {
    case MSG_FILE_NEW:
        _NewFile();
        break;
    case MSG_FILE_OPEN:
        _OpenFile();
        break;
    case MSG_FILE_SAVE:
        _SaveFile();
        break;
    case MSG_FILE_SAVE_AS:
        _SaveFileAs();
        break;
    case MSG_RUN_CODE:
        _RunCode();
        break;
    case MSG_CLEAR_OUTPUT:
        _ClearOutput();
        break;
    case MSG_CLEAR_CANVAS:
        _ClearCanvas();
        break;
    case MSG_LANG_BASIC:
        _SetLanguage("BASIC");
        break;
    case MSG_LANG_PILOT:
        _SetLanguage("PILOT");
        break;
    case MSG_LANG_LOGO:
        _SetLanguage("Logo");
        break;
    default:
        BWindow::MessageReceived(message);
        break;
    }
}

bool TimeWarpWindow::QuitRequested()
{
    if (fModified)
    {
        BAlert *alert = new BAlert("Unsaved Changes",
                                   "Save changes before closing?",
                                   "Cancel", "Don't Save", "Save",
                                   B_WIDTH_AS_USUAL, B_WARNING_ALERT);
        int32 result = alert->Go();

        if (result == 0) // Cancel
            return false;
        else if (result == 2) // Save
            _SaveFile();
    }

    be_app->PostMessage(B_QUIT_REQUESTED);
    return true;
}

void TimeWarpWindow::_RunCode()
{
    BString code = fEditor->GetText();
    if (code.Length() == 0)
    {
        fStatusBar->SetText("No code to run");
        return;
    }

    // TODO: Execute code based on fCurrentLanguage
    // For now, just echo to output
    fOutput->AppendText("🚀 Running ");
    fOutput->AppendText(fCurrentLanguage);
    fOutput->AppendText(" code...\n");
    fOutput->AppendText(code);
    fOutput->AppendText("\n");

    fStatusBar->SetText("Code executed");
}

void TimeWarpWindow::_ClearOutput()
{
    fOutput->Clear();
    fStatusBar->SetText("Output cleared");
}

void TimeWarpWindow::_ClearCanvas()
{
    fTurtleCanvas->Clear();
    fStatusBar->SetText("Canvas cleared");
}

void TimeWarpWindow::_NewFile()
{
    fEditor->SetText("");
    fCurrentFile = "";
    fModified = false;
    SetTitle("Time Warp IDE - New File");
    fStatusBar->SetText("New file created");
}

void TimeWarpWindow::_OpenFile()
{
    // TODO: Implement file panel
    fStatusBar->SetText("Open file - Not yet implemented");
}

void TimeWarpWindow::_SaveFile()
{
    if (fCurrentFile.Length() == 0)
    {
        _SaveFileAs();
        return;
    }
    // TODO: Implement file saving
    fStatusBar->SetText("Save - Not yet implemented");
}

void TimeWarpWindow::_SaveFileAs()
{
    // TODO: Implement save panel
    fStatusBar->SetText("Save As - Not yet implemented");
}

void TimeWarpWindow::_SetLanguage(const char *lang)
{
    fCurrentLanguage = lang;
    BString status("Language: ");
    status << lang;
    fStatusBar->SetText(status);
}
