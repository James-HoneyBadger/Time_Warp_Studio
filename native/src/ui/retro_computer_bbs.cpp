// SPDX-License-Identifier: MIT
//
// The HB-1 on the telephone: DIAL, the menu, reading, posting, hanging up.
//
// The board itself is `hb_bbs.cpp`. This file is the conversation -- the connect
// chatter, the handle, the one-letter menu of the period -- because that is a
// property of the machine's prompt and not of the messages.

#include "retro_computer_internal.hpp"

#include <QDateTime>
#include <QSettings>

namespace timewarp::ui {

using namespace retro_detail;

namespace {

/// The number the machine dials. 555-01xx is the block reserved for fiction, which
/// is exactly the right number for a board from a parallel 1982.
constexpr auto kBoardNumber = "555-0117";

constexpr auto kHandleSetting = "retroComputer/bbsHandle";

}  // namespace

void RetroComputerWindow::dialBoard() {
    text_tabs_->setCurrentWidget(tube_);
    appendConsole(QStringLiteral("ATDT %1").arg(QString::fromLatin1(kBoardNumber)));
    appendConsole(QStringLiteral("... DIALING ..."));

    if (!bbs_.load()) {
        // A board that cannot be read is a line that answered and then went quiet,
        // which is what a sysop's crashed machine sounded like.
        appendConsole(QStringLiteral("?NO CARRIER -- %1").arg(bbs_.error()));
        return;
    }

    appendConsole(QStringLiteral("CONNECT 300"));
    appendConsole(QStringLiteral("WELCOME TO %1").arg(bbs_.systemName()));
    appendConsole(QStringLiteral("RUN BY %1  \u00b7  %2 MESSAGE(S)")
                      .arg(bbs_.sysop())
                      .arg(bbs_.count()));

    QSettings settings;
    const QString remembered = settings.value(QLatin1String{kHandleSetting}).toString().trimmed();
    if (!remembered.isEmpty()) {
        // A board that already knows you does not ask again: the handle is the only
        // thing it remembers, and it is the caller's own name for themselves.
        handle_ = remembered;
        appendConsole(QStringLiteral("WELCOME BACK, %1.").arg(handle_));
        online_ = OnlineState::Menu;
        appendConsole(QStringLiteral("TYPE H FOR HELP, B FOR THE BULLETINS, BYE TO HANG UP."));
    } else {
        online_ = OnlineState::Handle;
        appendConsole(QStringLiteral("ENTER A HANDLE, OR BYE TO HANG UP:"));
    }
    updatePrompt();
}

void RetroComputerWindow::hangUp(const QString& reason) {
    if (online_ == OnlineState::Off) {
        return;
    }
    online_ = OnlineState::Off;
    post_body_.clear();
    post_subject_.clear();
    appendConsole(reason);
    appendConsole(QStringLiteral("READY."));
    updatePrompt();
}

bool RetroComputerWindow::handleOnlineLine(const QString& line) {
    const QString trimmed = line.trimmed();
    const QString word = headWord(trimmed).toUpper();

    if (online_ == OnlineState::Handle) {
        if (trimmed.isEmpty()) {
            appendConsole(QStringLiteral("?A HANDLE, PLEASE -- SOMETHING TO CALL YOU"));
            return true;
        }
        if (word == QLatin1String("BYE") || word == QLatin1String("HANGUP")) {
            hangUp(QStringLiteral("NO CARRIER"));
            return true;
        }
        handle_ = trimmed.left(16).toUpper();
        QSettings settings;
        settings.setValue(QLatin1String{kHandleSetting}, handle_);
        appendConsole(QStringLiteral("HELLO, %1.").arg(handle_));
        online_ = OnlineState::Menu;
        appendConsole(QStringLiteral("TYPE H FOR HELP, B FOR THE BULLETINS, BYE TO HANG UP."));
        updatePrompt();
        return true;
    }

    if (online_ == OnlineState::Posting) {
        // A line with one full stop ends the message, as it did on every board and
        // every mail system of the period. A blank line is a paragraph, not an end.
        if (trimmed == QLatin1String(".")) {
            finishPost();
            return true;
        }
        post_body_.append(line);
        return true;
    }

    if (trimmed.isEmpty()) {
        return true;
    }
    if (word == QLatin1String("H") || trimmed == QLatin1String("?")) {
        showOnlineHelp();
        return true;
    }
    if (word == QLatin1String("B") || word == QLatin1String("L") ||
        word == QLatin1String("BULLETINS")) {
        listBulletins();
        return true;
    }
    if (word == QLatin1String("R") || word == QLatin1String("READ")) {
        const QString argument = firstArgument(trimmed);
        bool ok = false;
        const int number = argument.toInt(&ok);
        if (!ok) {
            appendConsole(QStringLiteral("?R WHICH MESSAGE -- TYPE B FOR THE LIST"));
            return true;
        }
        readBulletin(number);
        return true;
    }
    if (word == QLatin1String("N") || word == QLatin1String("NUMBER")) {
        appendConsole(QStringLiteral("%1 MESSAGE(S) ON %2")
                          .arg(bbs_.count())
                          .arg(bbs_.systemName()));
        return true;
    }
    if (word == QLatin1String("P") || word == QLatin1String("POST")) {
        const QString subject = trimmed.mid(trimmed.indexOf(word, 0, Qt::CaseInsensitive) +
                                            word.size())
                                    .trimmed();
        post_subject_ = subject;
        post_body_.clear();
        online_ = OnlineState::Posting;
        appendConsole(QStringLiteral("SUBJECT: %1").arg(
            subject.isEmpty() ? QStringLiteral("(NO SUBJECT)") : subject));
        appendConsole(QStringLiteral("TYPE YOUR MESSAGE. A LINE WITH A SINGLE . ENDS IT."));
        return true;
    }
    if (word == QLatin1String("WHO")) {
        appendConsole(QStringLiteral("1 CALLER: %1").arg(handle_));
        return true;
    }
    if (word == QLatin1String("BYE") || word == QLatin1String("G") ||
        word == QLatin1String("Q") || word == QLatin1String("HANGUP")) {
        appendConsole(QStringLiteral("THANKS FOR CALLING, %1.").arg(handle_));
        hangUp(QStringLiteral("NO CARRIER"));
        return true;
    }
    appendConsole(QStringLiteral("?TYPE H FOR HELP"));
    return true;
}

void RetroComputerWindow::finishPost() {
    const QString body = post_body_.join(QLatin1Char('\n')).trimmed();
    if (body.isEmpty()) {
        appendConsole(QStringLiteral("?AN EMPTY MESSAGE IS NOT A MESSAGE"));
        post_body_.clear();
        online_ = OnlineState::Menu;
        return;
    }
    const QString stamp =
        QDateTime::currentDateTime().toString(QStringLiteral("yyyy-MM-dd HH:mm"));
    const int number = bbs_.post(handle_, post_subject_, body, stamp);
    post_body_.clear();
    post_subject_.clear();
    online_ = OnlineState::Menu;
    if (number <= 0) {
        appendConsole(QStringLiteral("?THE BOARD DID NOT TAKE IT -- %1").arg(bbs_.error()));
        return;
    }
    appendConsole(QStringLiteral("POSTED AS MESSAGE %1. THE SYSOP WILL SEE IT.").arg(number));
}

void RetroComputerWindow::listBulletins() {
    appendConsole(QStringLiteral("%1  \u00b7  %2 MESSAGE(S)")
                      .arg(bbs_.systemName())
                      .arg(bbs_.count()));
    for (const BbsMessage& message : bbs_.messages()) {
        appendConsole(QStringLiteral("%1  %2  %3  %4")
                          .arg(message.number, 3)
                          .arg(message.author.left(12).leftJustified(12),
                               message.subject.left(28).leftJustified(28),
                               message.posted));
    }
    appendConsole(QStringLiteral("TYPE R N TO READ ONE."));
}

void RetroComputerWindow::readBulletin(int number) {
    const BbsMessage message = bbs_.message(number);
    if (message.number == 0) {
        appendConsole(QStringLiteral("?NO MESSAGE %1 -- TYPE B FOR THE LIST").arg(number));
        return;
    }
    appendConsole(QStringLiteral("MESSAGE %1  \u00b7  %2  \u00b7  %3")
                      .arg(message.number)
                      .arg(message.subject, message.posted));
    appendConsole(QStringLiteral("FROM %1").arg(message.author));
    for (const QString& line : message.body.split(QLatin1Char('\n'))) {
        appendConsole(line);
    }
    appendConsole(QStringLiteral("END OF MESSAGE %1").arg(message.number));
}

void RetroComputerWindow::showOnlineHelp() {
    appendConsole(QStringLiteral("H            this page"));
    appendConsole(QStringLiteral("B            list the bulletins"));
    appendConsole(QStringLiteral("R N          read message N"));
    appendConsole(QStringLiteral("N            how many messages there are"));
    appendConsole(QStringLiteral("P [SUBJECT]  post; a line with a single . ends it"));
    appendConsole(QStringLiteral("WHO          who is online"));
    appendConsole(QStringLiteral("BYE          hang up"));
}

}  // namespace timewarp::ui
