// SPDX-License-Identifier: MIT
//
// The BASIC ROM's line editor: what a typed line means, AUTO, and renumbering.

#include "retro_computer_internal.hpp"

#include <algorithm>
#include <optional>

#include <QCloseEvent>
#include <QComboBox>
#include <QFileInfo>
#include <QFont>
#include <QFrame>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPalette>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollBar>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSplitter>
#include <QTabWidget>
#include <QTextCursor>
#include <QVBoxLayout>
#include <QWidget>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace timewarp::ui {

using namespace retro_detail;

namespace {

constexpr auto kNoProgram = "?NO PROGRAM";

}  // namespace

void RetroComputerWindow::handlePromptLine() {
    if (input_ == nullptr) {
        return;
    }
    const QString command = input_->text();
    input_->clear();
    input_->setFocus(Qt::ShortcutFocusReason);
    submitCommand(command);
}

void RetroComputerWindow::submitCommand(const QString& command) {
    if (!powered_) {
        // A machine with its switch off does not compute. Saying so on a dark screen
        // would be the very thing this release is against.
        return;
    }
    const QString trimmed = command.trimmed();
    // On the telephone, every line belongs to the board -- including a blank one,
    // which is a paragraph in a message. It has to come before the machine's own
    // words, or a caller typing B for "bulletins" would list the BASIC program.
    if (online_ != OnlineState::Off && handleOnlineLine(command)) {
        emit commandFinished();
        return;
    }
    if (trimmed.isEmpty()) {
        // An empty line is the way out of AUTO, which is the one thing a user types
        // at this prompt that means something precisely because it is empty.
        if (auto_active_) {
            endAuto();
            emit commandFinished();
        }
        return;
    }

    if (history_.isEmpty() || history_.last() != trimmed) {
        history_.append(trimmed);
    }
    applyHistoryDepth();
    history_index_ = static_cast<int>(history_.size());

    // Saying anything else after INIT's warning is a change of mind, whether it is
    // a machine word or a language command.
    if (headWord(trimmed) != QStringLiteral("INIT")) {
        init_pending_ = false;
    }

    if (voice_ != nullptr) {
        voice_->play(MachineVoice::Event::Key);
    }
    const RomInfo rom = romFor(language_);
    appendConsole(QStringLiteral("%1 %2")
                      .arg(QString::fromUtf8(rom.prompt), trimmed));

    if (language_ == Language::Basic && handleBasicLine(trimmed)) {
        emit commandFinished();
        return;
    }
    // Before the machine words, so `DELETE 20-30` reaches the line editor and
    // `DELETE "NAME"` falls through to HB DOS. `LIST 10-20` likewise; a bare `LIST`
    // returns false here and is the machine's word.
    if (language_ == Language::Basic && handleBasicEditor(trimmed)) {
        emit commandFinished();
        return;
    }
    if (handleMachineCommand(trimmed)) {
        emit commandFinished();
        return;
    }

    ensureRepl();
    if (repl_ != nullptr) {
        repl_->submit(trimmed);
    }
}

bool RetroComputerWindow::handleBasicLine(const QString& command) {
    static const QRegularExpression numbered{
        QStringLiteral(R"(^\s*(\d+)\s*(.*)$)")};
    const QRegularExpressionMatch match = numbered.match(command);
    if (!match.hasMatch()) {
        return false;
    }

    const QString number = match.captured(1);
    const QString body = match.captured(2).trimmed();
    // A bare `2 + 2` starts with a digit, but it is an expression, not line 2.
    // A program line starts with a keyword, the `?` shorthand or a comment mark.
    if (!body.isEmpty()) {
        const QChar first = body.front();
        if (!(first.isLetter() || first == QLatin1Char('?') ||
              first == QLatin1Char('\'') || first == QLatin1Char('#'))) {
            return false;
        }
    }

    QStringList lines = program_->toPlainText().split(QLatin1Char('\n'));
    int existing = -1;
    int insert_at = -1;
    const int wanted = number.toInt();
    for (int index = 0; index < lines.size(); ++index) {
        const QRegularExpressionMatch other = numbered.match(lines.at(index));
        if (!other.hasMatch()) {
            continue;
        }
        const int other_number = other.captured(1).toInt();
        if (other_number == wanted) {
            existing = index;
        }
        if (insert_at < 0 && other_number > wanted) {
            insert_at = index;
        }
    }

    if (body.isEmpty()) {
        if (existing < 0) {
            status_label_->setText(tr("?LINE %1 NOT FOUND").arg(number));
            return true;
        }
        lines.removeAt(existing);
        program_->setPlainText(lines.join(QLatin1Char('\n')));
        program_->document()->setModified(true);
        text_tabs_->setCurrentWidget(program_);
        status_label_->setText(tr("LINE %1 DELETED").arg(number));
        return true;
    }

    const QString stored = QStringLiteral("%1 %2").arg(number, body);
    if (existing >= 0) {
        lines[existing] = stored;
        insert_at = existing;
    } else {
        if (insert_at < 0) {
            insert_at = static_cast<int>(lines.size());
        }
        if (lines.size() == 1 && lines.front().trimmed().isEmpty()) {
            lines.clear();
            insert_at = 0;
        }
        lines.insert(insert_at, stored);
    }

    program_->setPlainText(lines.join(QLatin1Char('\n')));
    program_->document()->setModified(true);
    QTextCursor cursor{program_->document()};
    cursor.movePosition(QTextCursor::Start);
    for (int line = 0; line < insert_at; ++line) {
        cursor.movePosition(QTextCursor::Down);
    }
    program_->setTextCursor(cursor);
    text_tabs_->setCurrentWidget(program_);
    status_label_->setText(tr("LINE %1 STORED").arg(number));
    if (auto_active_) {
        advanceAuto(wanted);
    }
    return true;
}

// ---------------------------------------------------------------------------
// The BASIC ROM's line editor
QList<RetroComputerWindow::BasicLine> RetroComputerWindow::basicLines() const {
    QList<BasicLine> lines;
    if (program_ == nullptr) {
        return lines;
    }
    const QRegularExpression pattern = numberedLine();
    const QStringList text = program_->toPlainText().split(QLatin1Char('\n'));
    for (const QString& line : text) {
        const QRegularExpressionMatch match = pattern.match(line);
        if (!match.hasMatch()) {
            continue;
        }
        BasicLine entry;
        entry.number = match.captured(1).toInt();
        entry.body = match.captured(2).trimmed();
        lines.append(entry);
    }
    return lines;
}

void RetroComputerWindow::setBasicLines(const QList<BasicLine>& lines) {
    if (program_ == nullptr) {
        return;
    }
    QStringList text;
    text.reserve(lines.size());
    for (const BasicLine& line : lines) {
        text << (line.body.isEmpty() ? QString::number(line.number)
                                     : QStringLiteral("%1 %2").arg(line.number).arg(line.body));
    }
    program_->setPlainText(text.join(QLatin1Char('\n')));
    program_->document()->setModified(true);
}

bool RetroComputerWindow::renumberBasic(int start, int step, int* dangling) {
    const QList<BasicLine> lines = basicLines();
    if (lines.isEmpty()) {
        appendConsole(QString::fromLatin1(kNoProgram));
        return false;
    }
    QMap<int, int> renamed;
    for (int index = 0; index < lines.size(); ++index) {
        renamed.insert(lines.at(index).number, start + index * step);
    }
    QList<BasicLine> updated;
    int unresolved = 0;
    for (const BasicLine& line : lines) {
        BasicLine entry;
        entry.number = renamed.value(line.number);
        entry.body = renumberTargets(line.body, renamed, &unresolved);
        updated.append(entry);
    }
    setBasicLines(updated);
    if (dangling != nullptr) {
        *dangling = unresolved;
    }
    return true;
}

void RetroComputerWindow::startAuto(int start, int step) {
    auto_active_ = true;
    auto_next_ = start;
    auto_step_ = step <= 0 ? 10 : step;
    if (input_ != nullptr) {
        // The number is *in* the prompt, so the line the user types already carries
        // it and the ordinary numbered-line path stores it. AUTO needs no second
        // parser and the line looks the way a machine of the period looked.
        input_->setText(QString::number(auto_next_));
        input_->setCursorPosition(input_->text().size());
    }
    appendConsole(QStringLiteral("AUTO %1, %2   [AN EMPTY LINE ENDS IT]")
                      .arg(auto_next_)
                      .arg(auto_step_));
}

void RetroComputerWindow::endAuto() {
    auto_active_ = false;
    if (input_ != nullptr) {
        input_->clear();
    }
    appendConsole(QStringLiteral("READY."));
}

void RetroComputerWindow::advanceAuto(int stored) {
    if (!auto_active_) {
        return;
    }
    // Continue from the line that was just stored, so a numbered line typed in
    // the middle of an AUTO run moves the sequence rather than fighting it.
    auto_next_ = stored + auto_step_;
    if (input_ != nullptr) {
        input_->setText(QString::number(auto_next_));
        input_->setCursorPosition(input_->text().size());
    }
}

void RetroComputerWindow::rememberForUndo() {
    if (program_ == nullptr) {
        return;
    }
    const QString text = program_->toPlainText();
    undo_available_ = !text.trimmed().isEmpty();
    undo_text_ = undo_available_ ? text : QString{};
}

void RetroComputerWindow::restoreUndo() {
    if (!undo_available_ || program_ == nullptr) {
        appendConsole(QStringLiteral("?NOTHING TO UNDO"));
        return;
    }
    // The count is of the listing being put *back*, which is not what is on the
    // PROGRAM page now -- that is the empty page `NEW` left behind.
    int restored = 0;
    for (const QString& line : undo_text_.split(QLatin1Char('\n'))) {
        if (!line.trimmed().isEmpty()) {
            ++restored;
        }
    }
    program_->setPlainText(undo_text_);
    program_->document()->setModified(true);
    undo_available_ = false;
    undo_text_.clear();
    appendConsole(QStringLiteral("RESTORED %1 LINE(S)").arg(restored));
}

bool RetroComputerWindow::handleBasicEditor(const QString& command) {
    const QString trimmed = command.trimmed();
    const QString word = headWord(trimmed);
    const QString rest = trimmed.mid(word.size()).trimmed();

    if (word == QLatin1String("RENUM") || word == QLatin1String("RENUMBER")) {
        const QStringList parts = rest.split(QLatin1Char(','), Qt::SkipEmptyParts);
        bool ok = true;
        const int start = parts.isEmpty() ? 10 : parts.at(0).trimmed().toInt(&ok);
        if (!ok || start < 0) {
            appendConsole(QStringLiteral("?SYNTAX ERROR: RENUM [START[, STEP]]"));
            return true;
        }
        const int step =
            parts.size() > 1 ? parts.at(1).trimmed().toInt(&ok) : 10;
        if (!ok || step <= 0) {
            appendConsole(QStringLiteral("?SYNTAX ERROR: RENUM [START[, STEP]]"));
            return true;
        }
        int dangling = 0;
        if (!renumberBasic(start, step, &dangling)) {
            return true;
        }
        appendConsole(QStringLiteral("RENUMBERED FROM %1 BY %2").arg(start).arg(step));
        if (dangling > 0) {
            // Leaving an unresolvable target alone is the only honest choice: it was
            // already a runtime error and renumbering it would make it a different one.
            appendConsole(QStringLiteral("?%1 TARGET(S) NOT IN THE PROGRAM, LEFT ALONE")
                              .arg(dangling));
        }
        updateStatus();
        return true;
    }

    if (word == QLatin1String("AUTO")) {
        const QStringList parts = rest.split(QLatin1Char(','), Qt::SkipEmptyParts);
        bool ok = true;
        int start = 10;
        int step = 10;
        if (!parts.isEmpty()) {
            start = parts.at(0).trimmed().toInt(&ok);
        }
        if (ok && parts.size() > 1) {
            step = parts.at(1).trimmed().toInt(&ok);
        }
        if (!ok || start < 0 || step <= 0) {
            appendConsole(QStringLiteral("?SYNTAX ERROR: AUTO [START[, STEP]]"));
            return true;
        }
        // With no number given, continue after the listing: what a reader wants
        // when they type AUTO to add to a program they already have.
        if (parts.isEmpty()) {
            const QList<BasicLine> lines = basicLines();
            if (!lines.isEmpty()) {
                start = lines.last().number + 10;
            }
        }
        startAuto(start, step);
        return true;
    }

    if (word == QLatin1String("DELETE") || word == QLatin1String("DEL")) {
        const LineRange range = parseLineRange(rest);
        if (!range.valid) {
            // HB DOS's `DELETE "NAME"` is another word entirely, and reaches it.
            return false;
        }
        QList<BasicLine> lines = basicLines();
        int removed = 0;
        for (int index = lines.size(); index-- > 0;) {
            const int number = lines.at(index).number;
            if (number < range.first || number > range.last) {
                continue;
            }
            lines.removeAt(index);
            ++removed;
        }
        if (removed == 0) {
            appendConsole(QStringLiteral("?NO LINES IN RANGE"));
            return true;
        }
        setBasicLines(lines);
        appendConsole(QStringLiteral("%1 LINE(S) DELETED").arg(removed));
        updateStatus();
        return true;
    }

    if (word == QLatin1String("LIST") || word == QLatin1String("L")) {
        const LineRange range = parseLineRange(rest);
        if (!range.valid) {
            // A bare `LIST` is the machine's word, which every ROM has.
            return false;
        }
        text_tabs_->setCurrentWidget(tube_);
        int shown = 0;
        for (const BasicLine& line : basicLines()) {
            if (line.number < range.first || line.number > range.last) {
                continue;
            }
            appendConsole(QStringLiteral("%1 %2").arg(line.number).arg(line.body));
            ++shown;
        }
        if (shown == 0) {
            appendConsole(QStringLiteral("?NO LINES IN RANGE"));
        }
        return true;
    }

    if (word == QLatin1String("TRON") || word == QLatin1String("TROFF")) {
        trace_on_ = word == QLatin1String("TRON");
        appendConsole(trace_on_ ? QStringLiteral("TRACE ON")
                                : QStringLiteral("TRACE OFF"));
        return true;
    }

    if (word == QLatin1String("UNDO")) {
        restoreUndo();
        updateStatus();
        return true;
    }

    return false;
}

}  // namespace timewarp::ui
