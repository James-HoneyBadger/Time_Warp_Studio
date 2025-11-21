/*
 * Time Warp IDE - Haiku OS Edition
 * Output display view
 */

#ifndef OUTPUT_VIEW_H
#define OUTPUT_VIEW_H

#include <TextView.h>
#include <String.h>

class OutputView : public BTextView
{
public:
    OutputView(const char *name);
    virtual ~OutputView();

    void AppendText(const char *text);
    void Clear();

private:
    void _InitOutput();
};

#endif // OUTPUT_VIEW_H
