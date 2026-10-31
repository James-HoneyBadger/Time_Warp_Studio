// SPDX-License-Identifier: MIT

#include "timewarp/ui/terminal_text.hpp"

namespace timewarp::ui {

namespace {

/// How much of `bytes` ends on a UTF-8 character boundary.
///
/// A read can split a multi-byte character; decoding half of one shows a
/// replacement character instead of the letter that was meant. The tail is kept
/// and decoded with the next read.
[[nodiscard]] int completePrefix(const QByteArray& bytes) {
    const int size = static_cast<int>(bytes.size());
    int index = size - 1;
    int continuations = 0;
    while (index >= 0 && continuations < 3 &&
           (static_cast<unsigned char>(bytes.at(index)) & 0xC0) == 0x80) {
        --index;
        ++continuations;
    }
    if (index < 0) {
        return 0;
    }
    const auto lead = static_cast<unsigned char>(bytes.at(index));
    int needed = 1;
    if ((lead & 0x80) == 0x00) {
        needed = 1;
    } else if ((lead & 0xE0) == 0xC0) {
        needed = 2;
    } else if ((lead & 0xF0) == 0xE0) {
        needed = 3;
    } else if ((lead & 0xF8) == 0xF0) {
        needed = 4;
    } else {
        // Not a lead byte at all: let the decoder show what it makes of it.
        return index;
    }
    return index + needed <= size ? size : index;
}

}  // namespace

std::vector<TerminalOp> TerminalText::feed(const QByteArray& bytes) {
    std::vector<TerminalOp> ops;
    carry_.append(bytes);
    const int whole = completePrefix(carry_);
    if (whole <= 0) {
        // Nothing whole to decode. More than three bytes of an incomplete
        // character is not a character, so the tail is not worth keeping.
        if (carry_.size() > 3) {
            carry_.clear();
        }
        return ops;
    }
    const QString text = QString::fromUtf8(carry_.constData(), whole);
    carry_.remove(0, whole);
    for (const QChar character : text) {
        consume(character, ops);
    }
    flush(ops);
    return ops;
}

void TerminalText::reset() {
    state_ = State::Normal;
    pending_.clear();
    column_ = 0;
    pending_cr_ = false;
    carry_.clear();
}

void TerminalText::consume(QChar character, std::vector<TerminalOp>& ops) {
    switch (state_) {
        case State::Escape:
            if (character == QLatin1Char('[')) {
                state_ = State::Control;
            } else if (character == QLatin1Char(']')) {
                state_ = State::Osc;
            } else {
                // A two-character escape, `\e(` and its neighbours: nothing to show.
                state_ = State::Normal;
            }
            return;
        case State::Control:
            // A CSI sequence ends at the first byte in the final-byte range.
            if (character.unicode() >= 0x40 && character.unicode() <= 0x7E) {
                state_ = State::Normal;
            }
            return;
        case State::Osc:
            if (character == QChar{0x07}) {
                state_ = State::Normal;
            } else if (character == QChar{0x1B}) {
                state_ = State::OscEscape;
            }
            return;
        case State::OscEscape:
            // `\e\\` is the string terminator; anything else resumes the title.
            state_ = character == QLatin1Char('\\') ? State::Normal : State::Osc;
            return;
        case State::Normal:
            break;
    }

    if (pending_cr_) {
        pending_cr_ = false;
        if (character == QLatin1Char('\n')) {
            flush(ops);
            write(ops, QStringLiteral("\n"));
            column_ = 0;
            return;
        }
        // The line is about to be written again: take back what is on it.
        if (column_ > 0) {
            flush(ops);
            ops.push_back(TerminalOp{TerminalOp::Kind::EraseBack, QString{}, column_});
            column_ = 0;
        }
    }

    if (character == QChar{0x1B}) {
        flush(ops);
        state_ = State::Escape;
        return;
    }
    if (character == QLatin1Char('\r')) {
        flush(ops);
        pending_cr_ = true;
        return;
    }
    if (character == QLatin1Char('\n')) {
        flush(ops);
        write(ops, QStringLiteral("\n"));
        column_ = 0;
        return;
    }
    if (character == QChar{0x08}) {
        if (column_ > 0) {
            flush(ops);
            ops.push_back(TerminalOp{TerminalOp::Kind::EraseBack, QString{}, 1});
            --column_;
        }
        return;
    }
    if (character == QLatin1Char('\t')) {
        pending_.append(character);
        ++column_;
        return;
    }
    // The bell, NUL and the rest of the control range have no text to show.
    if (character.unicode() < 0x20 || character.unicode() == 0x7F) {
        return;
    }
    pending_.append(character);
    ++column_;
}

void TerminalText::flush(std::vector<TerminalOp>& ops) {
    write(ops, pending_);
    pending_.clear();
}

void TerminalText::write(std::vector<TerminalOp>& ops, const QString& text) {
    if (text.isEmpty()) {
        return;
    }
    ops.push_back(TerminalOp{TerminalOp::Kind::Write, text, 0});
}

}  // namespace timewarp::ui
