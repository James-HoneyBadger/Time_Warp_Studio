// SPDX-License-Identifier: MIT
//
// A pseudo-terminal's byte stream, turned into the text a person should read.
//
// A shell writes a stream, not text. Colour is `\e[01;32m`, a window title arrives
// as an OSC sequence, a progress bar rewrites its line with `\r`, and every line
// ends `\r\n` because the terminal's line discipline turns the `\n` into one. None
// of that is worth showing, and all of it would appear as litter in a text view.
//
// The filter is a value of its own so the rules can be tested without a shell: what
// arrives at the view is a list of edits -- append this, take that back -- and the
// widget does nothing but apply them in order.

#pragma once

#include <QByteArray>
#include <QString>

#include <vector>

namespace timewarp::ui {

/// One edit to make to the transcript, in the order the terminal emitted it.
struct TerminalOp {
    enum class Kind {
        Write,      ///< append `text` at the end
        EraseBack,  ///< remove `count` characters from the end
    };

    Kind kind = Kind::Write;
    QString text;
    int count = 0;
};

/// The filter between a pseudo-terminal and the transcript view.
class TerminalText {
public:
    /// Everything `bytes` contributes, as operations to apply in order.
    ///
    /// A trailing `\r` is held rather than applied: a `\r` followed by `\n` is just
    /// a line ending, and a `\r` followed by anything else is a line about to be
    /// rewritten. Which of the two it is is not known until the next byte arrives.
    [[nodiscard]] std::vector<TerminalOp> feed(const QByteArray& bytes);

    /// True when the last thing read was a carriage return whose next character has
    /// not arrived yet. For a test; the filter copes without being asked.
    [[nodiscard]] bool awaitingNewline() const { return pending_cr_; }

    /// Drops what has been read but not shown, so a replaced shell's first prompt
    /// does not erase the previous one's output.
    void reset();

private:
    enum class State {
        Normal,
        Escape,     ///< after `\e`, waiting for the kind
        Control,    ///< inside a CSI sequence: `\e[01;32m`
        Osc,        ///< inside an OSC string: `\e]0;title\a`
        OscEscape,  ///< inside an OSC string, after `\e`
    };

    void consume(QChar character, std::vector<TerminalOp>& ops);
    void flush(std::vector<TerminalOp>& ops);
    static void write(std::vector<TerminalOp>& ops, const QString& text);

    State state_ = State::Normal;
    /// Printable characters read but not yet emitted, so a line is one edit.
    QString pending_;
    /// Characters written since the last emitted newline: what a `\r` takes back.
    int column_ = 0;
    bool pending_cr_ = false;
    /// An incomplete UTF-8 character at the end of a read, kept for the next one.
    QByteArray carry_;
};

}  // namespace timewarp::ui
