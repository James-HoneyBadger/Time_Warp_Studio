// SPDX-License-Identifier: MIT
//
// The bulletin board on the other end of the HB-1's telephone line.
//
// A 1982 home computer with a modem had one thing the rest of this Studio does not:
// other people. The board is local -- a JSON file under `~/.time_warp/bbs` -- because
// a teaching machine must not need a network to demonstrate what a network was for,
// and because a board nobody can reach is a better classroom prop than a server
// nobody may connect to.
//
// What a caller does, in the idiom of the period: `DIAL` connects, a handle is asked
// for, and the menu offers the bulletins, a read, a post and a log-off. Posting is the
// classic two-step -- `P SUBJECT`, then lines, then a line with a single `.` -- so a
// message can be more than the one line the prompt holds.
//
// The first connection seeds a welcome message from the sysop, so a new board is not
// an empty room: an empty room teaches nothing about what a board was for.

#pragma once

#include <QList>
#include <QString>

namespace timewarp::ui {

/// One message on the board.
struct BbsMessage {
    int number = 0;
    QString author;
    QString subject;
    QString body;
    /// The wall clock it was posted at, as the board records it.
    QString posted;
};

/// The board itself: the messages and the file they live in.
///
/// No widget and no clock of its own: `post()` takes the timestamp as text, so a
/// test can assert exactly what a message says without waiting for a real minute to
/// pass.
class HbBbs {
public:
    /// `~/.time_warp/bbs/board.json`, honouring `TIME_WARP_DATA_DIR` as every other
    /// piece of machine state does.
    [[nodiscard]] static QString defaultBoardPath();

    /// Reads the board, seeding the sysop's welcome message when there is no file
    /// yet. A file that cannot be read is an empty board with `error` set: a caller
    /// that has just dialled in should be told, not shown nothing.
    [[nodiscard]] bool load(const QString& path = defaultBoardPath());

    [[nodiscard]] const QString& systemName() const noexcept { return system_name_; }
    [[nodiscard]] const QString& sysop() const noexcept { return sysop_; }
    [[nodiscard]] const QString& path() const noexcept { return path_; }
    [[nodiscard]] const QString& error() const noexcept { return error_; }
    [[nodiscard]] QList<BbsMessage> messages() const { return messages_; }
    [[nodiscard]] int count() const { return int(messages_.size()); }
    /// The message numbered `number`, or a default-constructed one when there is
    /// none. Callers check `number` rather than relying on an exception.
    [[nodiscard]] BbsMessage message(int number) const;

    /// Appends a message and writes the board. Returns the number it was given --
    /// one more than the last, which is how a caller says "message 4 posted" and how
    /// a board that has had messages deleted still reads in order. Zero means the
    /// write failed and nothing was kept.
    int post(const QString& author, const QString& subject, const QString& body,
             const QString& posted);

private:
    [[nodiscard]] bool save();

    QString path_;
    QString system_name_;
    QString sysop_;
    QString error_;
    QList<BbsMessage> messages_;
};

}  // namespace timewarp::ui
