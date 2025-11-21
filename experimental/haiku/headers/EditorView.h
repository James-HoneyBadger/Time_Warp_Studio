/*
 * Time Warp IDE - Haiku OS Edition
 * Code editor view (BTextView wrapper)
 */

#ifndef EDITOR_VIEW_H
#define EDITOR_VIEW_H

#include <TextView.h>
#include <String.h>

class EditorView : public BTextView
{
public:
    EditorView(const char *name);
    virtual ~EditorView();

    BString GetText() const;
    void SetText(const char *text);

private:
    void _InitEditor();
};

#endif // EDITOR_VIEW_H
