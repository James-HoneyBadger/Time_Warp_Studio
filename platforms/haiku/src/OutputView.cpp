/*
 * Time Warp IDE - Haiku OS Edition
 * Output view implementation
 */

#include "OutputView.h"

OutputView::OutputView(const char *name)
    : BTextView(name, B_WILL_DRAW | B_FRAME_EVENTS)
{
    _InitOutput();
}

OutputView::~OutputView()
{
}

void OutputView::_InitOutput()
{
    SetStylable(false);
    MakeEditable(false);
    SetWordWrap(true);

    // Set monospace font
    BFont font(be_fixed_font);
    font.SetSize(11.0);
    SetFontAndColor(&font);

    // Dark background for output console feel
    rgb_color bgColor = {20, 20, 25, 255};
    rgb_color fgColor = {220, 220, 220, 255};
    SetViewColor(bgColor);
    SetLowColor(bgColor);
    SetFontAndColor(&font, B_FONT_ALL, &fgColor);
}

void OutputView::AppendText(const char *text)
{
    int32 textLen = TextLength();
    Insert(textLen, text, strlen(text));
    ScrollToOffset(textLen);
}

void OutputView::Clear()
{
    SetText("");
}
