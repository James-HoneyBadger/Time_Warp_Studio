/*
 * Time Warp IDE - Haiku OS Edition
 * Code editor implementation
 */

#include "EditorView.h"
#include <ScrollView.h>

EditorView::EditorView(const char *name)
    : BTextView(name, B_WILL_DRAW | B_FRAME_EVENTS)
{
    _InitEditor();
}

EditorView::~EditorView()
{
}

void EditorView::_InitEditor()
{
    SetStylable(true);
    MakeEditable(true);
    SetWordWrap(false);

    // Set monospace font
    BFont font(be_fixed_font);
    font.SetSize(12.0);
    SetFontAndColor(&font);

    // Tab width = 4 spaces
    SetTabWidth(font.StringWidth("    "));
}

BString EditorView::GetText() const
{
    return BString(Text());
}

void EditorView::SetText(const char *text)
{
    BTextView::SetText(text);
}
